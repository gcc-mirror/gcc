// { dg-do compile { target aarch64*-*-* } }
// { dg-options "-Wclass-memaccess -msve-vector-bits=256" }

#pragma GCC target "+sve"

typedef __SIZE_TYPE__ size_t;
inline void *operator new (size_t, void *__p) throw() { return __p; }

#include <arm_sve.h>

typedef signed char int8x32_t __attribute__((__vector_size__ (32)));

// Sizeless objects with global scope.

svint8_t global_sve_sc; // { dg-error {SVE type 'svint8_t' does not have a fixed size} }
static svint8_t local_sve_sc; // { dg-error {SVE type 'svint8_t' does not have a fixed size} }
extern svint8_t extern_sve_sc; // { dg-error {SVE type 'svint8_t' does not have a fixed size} }
__thread svint8_t tls_sve_sc; // { dg-error {variables of type 'svint8_t' cannot have thread-local storage duration} }

// Sizeless arrays.

typedef svint8_t array_type[2]; // { dg-error {array elements cannot have SVE type 'svint8_t'} }
extern svint8_t extern_array[]; // { dg-error {array elements cannot have SVE type 'svint8_t'} }

// Sizeless member variables.

struct struct1 {
  svint8_t a; // { dg-error {member variables cannot have SVE type 'svint8_t'} }
};

union union1 {
  svint8_t a; // { dg-error {member variables cannot have SVE type 'svint8_t'} }
};

#if __cplusplus >= 201103L
struct static_sve_sc {
  static svint8_t sve_sc1 = {}; // { dg-error {SVE type 'svint8_t' does not have a fixed size} "" { target c++11 } }
};
#endif

// Sizeless member variables in templated structures.

template<typename T>
struct templated_struct1 {
  svint8_t a; // { dg-error {member variables cannot have SVE type 'svint8_t'} }
};

template<typename T>
struct templated_struct2 {
  T a; // { dg-error {member variables cannot have SVE type '(svint8_t|__SVInt8_t)'} }
};

template class templated_struct2<svint8_t>;

template<typename T>
struct templated_struct3 {
  T &a;
};

template class templated_struct3<svint8_t>;

#if __cplusplus >= 201103L
template<typename T>
struct templated_struct4 {
  static T a; // { dg-error {SVE type '(svint8_t|__SVInt8_t)' does not have a fixed size} "" { target c++11 } }
  static T b = {}; // { dg-error {SVE type '(svint8_t|__SVInt8_t)' does not have a fixed size} "" { target c++11 } }
};

template class templated_struct4<svint8_t>;
#endif

template<typename T> struct templated_struct5 : T {}; // { dg-error {base type '[^']*' fails to be a struct or class type} }
template class templated_struct5<svint8_t>;

template<typename T, unsigned N> struct templated_struct6 { T x[N]; }; // { dg-error {array elements cannot have SVE type '(__SVInt8_t|svint8_t)'} }
template class templated_struct6<svint8_t, 2>;

template<typename T>
struct templated_struct7 {
  static const int size = sizeof (T); // { dg-error {SVE type '(__SVInt8_t|svint8_t)' does not have a fixed size} }
#if __cplusplus >= 201103L
  static const int align = alignof (T); // { dg-error {SVE type '(__SVInt8_t|svint8_t)' does not have a defined alignment} "" { target c++11 } }
#endif

  void f1 (T (&)[2]); // { dg-error {array elements cannot have SVE type '(__SVInt8_t|svint8_t)'} }
#if __cplusplus >= 201103L
  auto f2 () -> decltype (new T); // { dg-error {cannot allocate objects with SVE type '(__SVInt8_t|svint8_t)'} "" { target c++11 } }
  auto f3 (T *a) -> decltype (delete a); // { dg-error {cannot delete objects with SVE type '(__SVInt8_t|svint8_t)'} "" { target c++11 } }
#else
  void f2 () throw (T); // { dg-error {cannot throw or catch SVE type '(__SVInt8_t|svint8_t)'} "" { target c++98_only } }
#endif
};
template class templated_struct7<svint8_t>;

template<typename T> struct templated_struct8 { typedef int type; };

template<typename T>
void sfinae_f1 (typename templated_struct8<T[2]>::type);
template<typename T>
void sfinae_f1 (T &);

#if __cplusplus >= 201103L
template<int N> using typedef_sizeless1 = svint8_t;
template<int N> using typedef_sizeless1 = svint8_t;
template<typename T> using array = T[2]; // { dg-error {array elements cannot have SVE type '(svint8_t|__SVInt8_t)'} "" { target c++11 } }
#endif

// Pointers to sizeless types.

svint8_t *global_sve_sc_ptr;

// Sizeless arguments and return values.

void ext_consume_sve_sc (svint8_t);
void ext_consume_const_int_ref (const int &);
void ext_consume_varargs (int, ...);
svint8_t ext_produce_sve_sc ();

// Sizeless types in throw specifications.

#if __cplusplus < 201103L
void thrower1 () throw (svint8_t); // { dg-error {cannot throw or catch SVE type 'svint8_t'} "" { target c++98_only } }
void thrower2 () throw (svint8_t); // { dg-error {cannot throw or catch SVE type 'svint8_t'} "" { target c++98_only } }
void thrower3 () throw (svint8_t); // { dg-error {cannot throw or catch SVE type 'svint8_t'} "" { target c++98_only } }
#endif

extern int bar (void);

// Main tests for statements and expressions.

void
statements (int n)
{
  // Local declarations.

  svint8_t sve_sc1, sve_sc2;
  volatile svint8_t volatile_sve_sc1;
  int8x32_t gnu_sc1;
  svint16_t sve_sh1;

  // Layout queries.

  sizeof (svint8_t); // { dg-error {SVE type 'svint8_t' does not have a fixed size} }
  sizeof (sve_sc1); // { dg-error {SVE type 'svint8_t' does not have a fixed size} }
  sizeof (ext_produce_sve_sc ()); // { dg-error {SVE type 'svint8_t' does not have a fixed size} }
  __alignof (svint8_t); // { dg-error {SVE type 'svint8_t' does not have a defined alignment} }
  __alignof (sve_sc1); // { dg-error {SVE type 'svint8_t' does not have a defined alignment} }
  __alignof (ext_produce_sve_sc ()); // { dg-error {SVE type 'svint8_t' does not have a defined alignment} }

#if __cplusplus >= 201103L
  array<svint8_t> foo = {}; // { dg-message {required from here} "" { target c++11 } }
#endif

  // Initialization.

  int init_int1 = sve_sc1; // { dg-error {cannot convert 'svint8_t' to 'int' in initialization} }
  int init_int2 = { sve_sc1 }; // { dg-error {cannot convert 'svint8_t' to 'int' in initialization} }

  svint8_t init_sve_sc1 (sve_sc1);
  svint8_t init_sve_sc2 (sve_sh1); // { dg-error {cannot convert 'svint16_t' to 'svint8_t'} }
  svint8_t init_sve_sc3 = sve_sc1;
  svint8_t init_sve_sc4 = sve_sh1; // { dg-error {cannot convert 'svint16_t' to 'svint8_t'} }
  svint8_t init_sve_sc5 = {};
  svint8_t init_sve_sc6 = { sve_sc1 }; // { dg-error {cannot convert 'svint8_t' to 'signed char'} "" { target c++98_only } }
  svint8_t init_sve_sc7 = { sve_sh1 }; // { dg-error {cannot convert 'svint16_t' to 'signed char'} }
  svint32_t init_sve_vc1 = { 0, 1 };
  svint32_t init_sve_vc2 = { 0, bar () };
  svint32_t init_sve_vc3 = { bar (), n };
  svint32_t init_sve_vc4 = { 0, 1, 2, 3, 4, 5, 6, 7 };
  svint32_t init_sve_vc5 = { 0, 1, bar (), 3, 4, 5, n, 7 };
  svint32_t init_sve_vc6 = { 0, 1, 2, 3, 4, 5, 6, 7, 8 }; // { dg-error {too many initializers for 'svint32_t'} }
  svint32_t init_sve_vc7 = { 0, 1, 2, 3, bar (), 5, 6, 7, n }; // { dg-error {too many initializers for 'svint32_t'} }
  svint32_t init_sve_vc8 = { 0, bar (), 2, 3, 4, n, 5, 6, 7, 8, 9 }; // { dg-error {too many initializers for 'svint32_t'} }
  svint32_t init_sve_vc9 = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }; // { dg-error {too many initializers for 'svint32_t'} }

  // Constructor calls.

  (0, svint8_t ());

  // Lvalue reference binding

  svint8_t &lvalue_ref_sve_sc1 = sve_sc1;
  svint8_t &lvalue_ref_sve_sc2 = ext_produce_sve_sc (); // { dg-error {cannot bind non-const lvalue reference of type 'svint8_t&' to an rvalue of type 'svint8_t'} }
  svint8_t &lvalue_ref_sve_sc3 = sve_sh1; // { dg-error {invalid initialization of reference of type 'svint8_t&' from expression of type 'svint16_t'} }

  const svint8_t &const_lvalue_ref_sve_sc1 = sve_sc1;
  const svint8_t &const_lvalue_ref_sve_sc2 = ext_produce_sve_sc ();
  const svint8_t &const_lvalue_ref_sve_sc3 = sve_sh1; // { dg-error {invalid initialization of reference of type 'const svint8_t&' from expression of type 'svint16_t'} }

  // Compound literals.

  (int) { sve_sc1 }; // { dg-error {cannot convert 'svint8_t' to 'int' in initialization} }

  // Arrays.

  svint8_t array[2]; // { dg-error {array elements cannot have SVE type 'svint8_t'} }
  svint8_t zero_length_array[0]; // { dg-error {array elements cannot have SVE type 'svint8_t'} }
  svint8_t empty_init_array[] = {}; // { dg-error {array elements cannot have SVE type 'svint8_t'} }

  // Assignment.

  n = sve_sc1; // { dg-error {cannot convert 'svint8_t' to 'int' in assignment} }

  sve_sc1 = 0; // { dg-error {cannot convert 'int' to 'svint8_t' in assignment} }
  sve_sc1 = sve_sc1;
  sve_sc1 = gnu_sc1;
  gnu_sc1 = sve_sc1;
  sve_sc1 = sve_sh1; // { dg-error {cannot convert 'svint16_t' to 'svint8_t'} }

  // Casting.

  (void) sve_sc1;
  (void) volatile_sve_sc1;
  (void) *&volatile_sve_sc1;

  // Addressing and dereferencing.

  svint8_t *sve_sc_ptr = &sve_sc1;
  int8x32_t *gnu_sc_ptr = &gnu_sc1;
  sve_sc_ptr = (svint16_t *) 0; // { dg-error {cannot convert 'svint16_t\*' to 'svint8_t\*' in assignment} }

  // Pointer assignment.

  gnu_sc_ptr = sve_sc_ptr; // { dg-error {invalid conversion from 'svint8_t\*' to 'int8x32_t\*'} }
  sve_sc_ptr = gnu_sc_ptr; // { dg-error {invalid conversion from 'int8x32_t\*'[^\n]* to 'svint8_t\*'} }

  // Pointer arithmetic.

  ++sve_sc_ptr; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  --sve_sc_ptr; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr++; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr--; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr += 0; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr += 1; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr -= 0; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr -= 1; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc_ptr - sve_sc_ptr; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  gnu_sc_ptr - sve_sc_ptr; // { dg-error {invalid operands of types 'int8x32_t\*'[^\n]* and 'svint8_t\*' to binary 'operator-'} }
  sve_sc_ptr - gnu_sc_ptr; // { dg-error {invalid operands of types 'svint8_t\*' and 'int8x32_t\*'[^\n]* to binary 'operator-'} }
  sve_sc1 = sve_sc_ptr[0]; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  sve_sc1 = sve_sc_ptr[1]; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }

  // Pointer comparison.

  sve_sc_ptr == &sve_sc1;
  sve_sc_ptr != &sve_sc1;
  sve_sc_ptr < &sve_sc1;
  sve_sc_ptr <= &sve_sc1;
  sve_sc_ptr > &sve_sc1;
  sve_sc_ptr >= &sve_sc1;
  gnu_sc_ptr == sve_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  gnu_sc_ptr != sve_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  gnu_sc_ptr < sve_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  gnu_sc_ptr <= sve_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  gnu_sc_ptr > sve_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  gnu_sc_ptr >= sve_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  sve_sc_ptr == gnu_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  sve_sc_ptr != gnu_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  sve_sc_ptr < gnu_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  sve_sc_ptr <= gnu_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  sve_sc_ptr > gnu_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }
  sve_sc_ptr >= gnu_sc_ptr; // { dg-error {comparison between distinct pointer types [^\n]*lacks a cast} }

  // New and delete.

  new svint8_t; // { dg-error {cannot allocate objects with SVE type 'svint8_t'} }
  new svint8_t (); // { dg-error {cannot allocate objects with SVE type 'svint8_t'} }

  new (global_sve_sc_ptr) svint8_t; // { dg-error {cannot allocate objects with SVE type 'svint8_t'} }
  new (global_sve_sc_ptr) svint8_t (); // { dg-error {cannot allocate objects with SVE type 'svint8_t'} }

  sve_sc1.~svint8_t(); // { dg-error {expected class-name before '\(' token} }
  delete sve_sc_ptr; // { dg-error {cannot delete objects with SVE type '(svint8_t|__SVInt8_t)'} }
  delete[] sve_sc_ptr; // { dg-error {cannot delete objects with SVE type 'svint8_t'} }

  // Conditional expressions.

  0 ? sve_sc1 : sve_sc1;
  0 ? sve_sc1 : sve_sh1; // { dg-error {different types 'svint8_t' and 'svint16_t'} }
  0 ? sve_sc1 : 0; // { dg-error {different types 'svint8_t' and 'int'} }
  0 ? 0 : sve_sc1; // { dg-error {different types 'int' and 'svint8_t'} }
  0 ? sve_sc1 : sve_sc1;
  0 ? sve_sc_ptr : sve_sc_ptr;
  0 ? sve_sc_ptr : gnu_sc_ptr; // { dg-error {conditional expression between distinct pointer types [^\n]*lacks a cast} }
  0 ? gnu_sc_ptr : sve_sc_ptr; // { dg-error {conditional expression between distinct pointer types [^\n]*lacks a cast} }

  // Function arguments.

  ext_consume_sve_sc (sve_sc1);
  ext_consume_sve_sc (sve_sh1); // { dg-error {cannot convert 'svint16_t' to 'svint8_t'} }
  ext_consume_const_int_ref (sve_sc1); // { dg-error {invalid initialization of reference of type 'const int&' from expression of type 'svint8_t'} }
  ext_consume_varargs (sve_sc1); // { dg-error {cannot convert 'svint8_t' to 'int'} }
  ext_consume_varargs (1, sve_sc1);

  // Function returns.

  ext_produce_sve_sc ();
  sve_sc1 = ext_produce_sve_sc ();
  sve_sh1 = ext_produce_sve_sc (); // { dg-error {cannot convert 'svint8_t' to 'svint16_t' in assignment} }

  // Auto

#if __cplusplus >= 201103L
  auto auto_sve_sc1 = sve_sc1;
  auto auto_sve_sc1_return = ext_produce_sve_sc ();
#endif

  // Varargs processing.

  __builtin_va_list valist;
  __builtin_va_arg (valist, svint8_t);

  // Other built-ins

  __builtin_launder (sve_sc1); // { dg-error {'svint8_t' of argument to '__builtin_launder' is not a pointer to object type} }
  __builtin_memcpy (&sve_sc1, &sve_sc2, 2);

  // Lambdas

#if __cplusplus >= 201103L
  [sve_sc1] () {}; // { dg-error {capture by copy of SVE type 'svint8_t'} "" { target c++11 } }
  [=] () { &sve_sc1; }; // { dg-error {capture by copy of SVE type 'svint8_t'} "" { target c++11 } }
  [&sve_sc1] () { sve_sc1 = sve_sc2; }; // { dg-error {'sve_sc2' is not captured} "" { target c++11 } }
  [&sve_sc1, &sve_sc2] () { sve_sc1 = sve_sc2; };
  [&] () { sve_sc1 = sve_sc2; };
  [] () { return ext_produce_sve_sc (); } ();
#endif

  // Exceptions

  throw svint8_t (); // { dg-error {cannot throw or catch SVE type 'svint8_t'} }
  try {} catch (svint8_t x) {} // { dg-error {cannot throw or catch SVE type 'svint8_t'} }
  try {} catch (svint8_t &x) {} // { dg-error {cannot throw or catch SVE type 'svint8_t'} }
  try {} catch (svint8_t *x) {}
#if __cplusplus < 201103L
  thrower2 ();
#endif

  sfinae_f1<svint8_t> (sve_sc1);

  // Use in traits.  Doesn't use static_assert so that tests work with
  // earlier -std=s.

  { typedef int f[__has_nothrow_assign (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_trivial_assign (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_nothrow_constructor (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_trivial_constructor (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_nothrow_copy (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_trivial_copy (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_trivial_destructor (svint8_t) ? 1 : -1]; }
  { typedef int f[__has_unique_object_representations (svint8_t) ? 1 : -1]; }
  { typedef int f[!__has_virtual_destructor (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_abstract (svint8_t) ? 1 : -1]; }
  { typedef int f[__is_aggregate (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_base_of (svint8_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_base_of (svint8_t, svint16_t) ? 1 : -1]; }
  { typedef int f[!__is_class (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_empty (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_enum (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_final (svint8_t) ? 1 : -1]; }
  { typedef int f[__is_pod (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_polymorphic (svint8_t) ? 1 : -1]; }
  { typedef int f[__is_same_as (svint8_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_same_as (svint8_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[!__is_same_as (int8x32_t, svint8_t) ? 1 : -1]; }
  { typedef int f[__is_same_as (svint8_t *, svint8_t *) ? 1 : -1]; }
  { typedef int f[!__is_same_as (svint8_t *, int8x32_t *) ? 1 : -1]; }
  { typedef int f[!__is_same_as (int8x32_t *, svint8_t *) ? 1 : -1]; }
  { typedef int f[!__is_same_as (svint8_t, int) ? 1 : -1]; }
  { typedef int f[!__is_same_as (svint8_t, svint16_t) ? 1 : -1]; }
  { typedef int f[__is_trivial (svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_union (svint8_t) ? 1 : -1]; }
  { typedef int f[__is_trivially_copyable (svint8_t) ? 1 : -1]; }
  /* The intention is that svint8_t should behave like int8x32_t here.  If the behavior
     for int8x32_t changes then the behavior for svint8_t should change in the same
     way.  */
  { typedef int f[!__is_trivially_assignable (int8x32_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[!__is_trivially_assignable (svint8_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_trivially_assignable (svint8_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[!__is_trivially_assignable (int8x32_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_trivially_assignable (svint8_t, int) ? 1 : -1]; }
  { typedef int f[!__is_trivially_assignable (svint8_t, svint16_t) ? 1 : -1]; }
  { typedef int f[!__is_assignable (int8x32_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[!__is_assignable (svint8_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_assignable (svint8_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[!__is_assignable (int8x32_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_assignable (svint8_t, int) ? 1 : -1]; }
  { typedef int f[!__is_assignable (svint8_t, svint16_t) ? 1 : -1]; }
  { typedef int f[__is_trivially_constructible (svint8_t) ? 1 : -1]; }
  { typedef int f[__is_trivially_constructible (int8x32_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[__is_trivially_constructible (svint8_t, svint8_t) ? 1 : -1]; }
  { typedef int f[__is_trivially_constructible (svint8_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[__is_trivially_constructible (int8x32_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_trivially_constructible (svint8_t, int) ? 1 : -1]; }
  { typedef int f[!__is_trivially_constructible (svint8_t, svint16_t) ? 1 : -1]; }
  { typedef int f[__is_constructible (svint8_t) ? 1 : -1]; }
  { typedef int f[__is_constructible (int8x32_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[__is_constructible (svint8_t, svint8_t) ? 1 : -1]; }
  { typedef int f[__is_constructible (svint8_t, int8x32_t) ? 1 : -1]; }
  { typedef int f[__is_constructible (int8x32_t, svint8_t) ? 1 : -1]; }
  { typedef int f[!__is_constructible (svint8_t, int) ? 1 : -1]; }
  { typedef int f[!__is_constructible (svint8_t, svint16_t) ? 1 : -1]; }
}

// Function parameters in definitions.

void
unnamed_st1 (svint8_t)
{
}

void
named_st1 (svint8_t param1)
{
  svint8_t sve_sc1 = param1;
}

// Function return values in definitions.

svint8_t
ret_st1 (svint8_t param)
{
  return param;
}

svint8_t
bad_ret_st1 (svint16_t param)
{
  return param; // { dg-error {cannot convert 'svint16_t' to 'svint8_t' in return} }
}

#if __cplusplus >= 201103L
template<typename T>
void
const_to_sve_sc (T i)
{
  constexpr svint8_t a = (svint8_t) i;
}
#endif

template<typename T>
int
get_x (T *a)
{
  return a->a; // { dg-error {request for member 'a' in '\* a', which is of non-class type} }
}
template int get_x<svint8_t>(svint8_t *);

#if __cplusplus < 201103L
void thrower3 () throw (svint8_t) {} // { dg-error {cannot throw or catch SVE type 'svint8_t'} "" { target c++98_only } }
#endif

// Using "auto" as a return type.

#if __cplusplus >= 201402L
auto auto_ret_sve_sc (svint8_t *ptr) { return *ptr; }
const auto &auto_ret_const_sve_sc_ref (svint8_t *ptr) { return *ptr; }
auto &auto_ret_sve_sc_ref (svint8_t *ptr) { return *ptr; }
auto &&auto_ret_sve_sc_rvalue_ref (svint8_t *ptr) { return *ptr; }
#endif
