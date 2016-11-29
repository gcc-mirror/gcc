// Test exercising SFINAE depending on the well-definedness of constexpr
// functions.
// { dg-do compile { target c++14 } }

#define Assert(e) static_assert ((e), #e)

// Exercise SFINAE based on the absence of integer division by zero.
namespace DivByZero {

// Define a pair of functions that have undefined and well-defined
// behavior, respectively, due to division by zero, depending on
// their arguments.

// The following function is undefined when I is zero, well defined
// otherwise.
constexpr bool div_zero_0 (int i, int j) { return 1 + j / (i == 0); }

// The following function is undefined when I is non-zero, and well
// defined otherwise.
constexpr bool div_zero_1 (int i, int j) { return 1 + j / (i != 0); }

// Define a pair of overfloads each of which is viable when the constexpr
// function it invokes has well-defined semantics and not otherwise.
template <int I>
constexpr int f (int (*)[div_zero_0 (I, 0)] = 0) { return 0; }

template <int I>
constexpr int f (int (*)[div_zero_1 (I, 0)] = 0) { return 1; }

// Verify that the correct overload is selected based on the template
// argument and without triggering a compilation error for the undefined
// behavior in the non-viable constexpr function above.
Assert (f<0>() == 0);
Assert (f<1>() == 1);

}

// Exercise SFINAE based on the absence of signed integer overflow
// in addition.
namespace IntAddOverflow {

constexpr int a [] = { 1234, __INT_MAX__ / 2 };

constexpr int vflow_0 (int i) { return a [!i] * 7; }
constexpr int vflow_1 (int i) { return a [i] * 11; }

template <int I>
constexpr int f (int (*)[vflow_0 (I)] = 0) { return 1; }

template <int I>
constexpr int f (int (*)[vflow_1 (I)] = 0) { return 0; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 0);
Assert (n1 == 1);

}

// Exercise SFINAE based on the absence of signed integer overflow
// in multiplication.
namespace IntMulOverflow {

constexpr long a [] = { 1234, __LONG_MAX__ / 2 };

constexpr long vflow_0 (int i) { return a [!i] * 3; }
constexpr long vflow_1 (int i) { return a [i] * 7; }

template <int I>
constexpr int f (int (*)[vflow_0 (I)] = 0) { return 1; }

template <int I>
constexpr int f (int (*)[vflow_1 (I)] = 0) { return 0; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 0);
Assert (n1 == 1);

}

// Exercise SFINAE based on the absence of undefined pointer arithmetic
// involving null pointers.  Subtracting one null pointer from another
// is well-defined, but subtracting a null pointer from a non-null one
// is not.
namespace NullPointerArithmetic {

constexpr int i = 0;
constexpr const int* a[] = { 0, &i };

// Well-defined core constant expressions involving null pointers.
constexpr __PTRDIFF_TYPE__ d00 = a [0] - a [0];
constexpr __PTRDIFF_TYPE__ d11 = a [1] - a [1];

// Undefined core constant expressions involving null pointers.
// constexpr __PTRDIFF_TYPE__ d01 = a [0] - a [1];
// constexpr __PTRDIFF_TYPE__ d10 = a [1] - a [0];

// Valid when i == j.
constexpr bool
nullptr_sub_0 (bool i, bool j) { return 1 + a [!i] - a [!j]; }

// Valid when i != j.
constexpr bool
nullptr_sub_1 (bool i, bool j) { return 1 + a [i] - a [!j]; }

// Selected when I == 0.
template <bool I>
constexpr int f (int (*)[nullptr_sub_0 (I, 0)] = 0) { return 0; }

// Selected when I != 0.
template <bool I>
constexpr int f (int (*)[nullptr_sub_1 (I, 0)] = 0) { return 1; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 0);
Assert (n1 == 1);

}

// Exercise SFINAE based on the absence of undefined pointer arithmetic
// involving null poiinters.  Subtracting one null pointer from another
// is well-defined, but subtracting a null pointer from a non-null one
// is not.
namespace NullPointerDereference {

struct S { int a, b; };

constexpr S s = { };
constexpr const S* a[] = { 0, &s };

constexpr bool nullptr_ref_0 (int i) { return &a [i != 0]->b == &s.b; }
constexpr bool nullptr_ref_1 (int i) { return &a [i == 0]->b == &s.b; }

template <int I>
constexpr int f (int (*)[nullptr_ref_0 (I)] = 0) { return 1; }

template <int I>
constexpr int f (int (*)[nullptr_ref_1 (I)] = 0) { return 0; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 0);
Assert (n1 == 1);

}

// Exercise SFINAE based on whether or not two constexpr function
// calls have a circular depency on one another such that a call
// to one would not terminate.
namespace CircularDependency {

constexpr bool call_me (int i, bool (*f)(int)) { return f (i); }

constexpr bool undefined_if_0 (int i) {
    return i ? 1 : call_me (i, undefined_if_0);
}

constexpr bool undefined_if_1 (int i) {
    return i ? call_me (i, undefined_if_1) : 1;
}

template <int I>
constexpr int f (int (*)[undefined_if_0 (I)] = 0) { return 0; }

template <int I>
constexpr int f (int (*)[undefined_if_1 (I)] = 0) { return 1; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 1);
Assert (n1 == 0);

}

// Exercise SFINAE based on whether constexpr functions flow off
// the end without returning a value.
namespace FlowOffTheEnd {

constexpr bool undefined_if_0 (int i) { switch (i) case 1: return 1; }
constexpr bool undefined_if_1 (int i) { switch (i) case 0: return 1; }

template <int I>
constexpr int f (int (*)[undefined_if_0 (I)] = 0) { return 1; }

template <int I>
constexpr int f (int (*)[undefined_if_1 (I)] = 0) { return 0; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 0);
Assert (n1 == 1);

}

// Exercise SFINAE based on the presence and absence of a left shift
// expression with a negative second operand.
namespace NegativeLeftShift {

constexpr int a [] = { -1, 1 };

constexpr int undefined_if_0 (int i) { return 1 << a [i]; }
constexpr int undefined_if_1 (int i) { return 1 << a [!i]; }

template <int I>
constexpr int f (int (*)[undefined_if_0 (I)] = 0) { return 0; }

template <int I>
constexpr int f (int (*)[undefined_if_1 (I)] = 0) { return 1; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 1);
Assert (n1 == 0);

}

// Exercise SFINAE based on the presence and absence of a right shift
// expression with a negative second operand.
namespace NegativeRightShift {

constexpr int a [] = { -1, 1 };

constexpr int undefined_if_0 (int i) { return 2 >> a [i]; }
constexpr int undefined_if_1 (int i) { return 2 >> a [!i]; }

template <int I>
constexpr int f (int (*)[undefined_if_0 (I)] = 0) { return 0; }

template <int I>
constexpr int f (int (*)[undefined_if_1 (I)] = 0) { return 1; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 1);
Assert (n1 == 0);

}

// Exercise SFINAE based on the absence of signed integer overflow
// in a signed left shift expression.
namespace LeftShiftOverflow {

constexpr int a[] = { 1234, 1 };

constexpr int undefined_if_0 (int i) { return 1 << a [i]; }
constexpr int undefined_if_1 (int i) { return 1 << a [!i]; }

template <int I>
constexpr int f (int (*)[undefined_if_0 (I)] = 0) { return 0; }

template <int I>
constexpr int f (int (*)[undefined_if_1 (I)] = 0) { return 1; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 1);
Assert (n1 == 0);

}

// Exercise SFINAE based on the absence of using a negative array
// index.
namespace NegativeArrayIndex {

constexpr int a [] = { -1, 1 };

constexpr int undefined_if_0 (int i) { return 2 + a [a [i]]; }
constexpr int undefined_if_1 (int i) { return 2 + a [a [!i]]; }

template <int I>
constexpr int f (int (*)[undefined_if_0 (I)] = 0) { return 0; }

template <int I>
constexpr int f (int (*)[undefined_if_1 (I)] = 0) { return 1; }

constexpr int n0 = f<0>();
constexpr int n1 = f<1>();

Assert (n0 == 1);
Assert (n1 == 0);

}
