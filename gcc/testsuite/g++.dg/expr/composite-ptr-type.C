// DR 1512
// Test the composite pointer type of two operands.
// { dg-do compile { target c++11 } }

using nullptr_t = decltype(nullptr);

template <class T, class U> struct same;
template <class T> struct same<T,T> { };

template<typename T>
T fn ()
{
}

// Check that the composite pointer type of T and U is RES.
template<typename T, typename U, typename RES>
void test ()
{
  same<decltype(true ? fn<T>() : fn<U>()), RES> s;
}

struct A { };
struct B : A { };

// Test [expr.type]/3.
void
foo ()
{
  // if both p1 and p2 are null pointer constants -> std::nullptr_­t.
  test<nullptr_t, nullptr_t, nullptr_t>();

  // if either p1 or p2 is a null pointer constant -> T2 or T1.
  test<nullptr_t, const char **, const char **>();
  test<const char **, nullptr_t, const char **>();

  // if T1 or T2 is 'pointer to cv1 void' and the other type is 'pointer
  // to cv2 T', where T is an object type or void -> 'pointer to cv12 void',
  // where cv12 is the union of cv1 and cv2.
  test<const int *, volatile void *, const volatile void *>();
  test<const void *, volatile int *, const volatile void *>();

  test<int *, const int *, const int *>();
  // Make sure that we propagate 'const' here as per [conv.qual]/3.3.
  test<int **, const int **, const int *const *>();
  test<int *volatile *, const int **, const int *const volatile *>();
  test<int **, volatile int **, volatile int *const *>();

  // if T1 is 'pointer to cv1 C1' and T2 is 'pointer to cv2 C2', where C1 is
  // reference-related to C2 or C2 is reference-related to C1 -> the cv-combined
  // type of T1 and T2 or the cv-combined type of T2 and T1, respectively.
  test<const A*, volatile B*, const volatile A *>();
  test<const B*, volatile A*, const volatile A *>();

  test<const int *A::*, volatile int *A::*, const volatile int *const A::*>();
  // FIXME: This doesn't work if they're reference-related but not same.
  //test<const int *A::*, volatile int *B::*, const volatile int *const B::*>();
  //test<const int *B::*, volatile int *A::*, const volatile int *const B::*>();

  // if T1 or T2 is 'pointer to noexcept function' and the other type is
  // 'pointer to function', where the function types are otherwise the same
  // -> 'pointer to function'.
  test<int (*)() noexcept, int (*)(), int (*)()>();
  test<int (*)(), int (*)() noexcept, int (*)()>();

  // if T1 or T2 is 'pointer to member of C1 of type function', the other type
  // is 'pointer to member of C2 of type noexcept function', and C1 is
  // reference-related to C2 or C2 is reference-related to C1, where the
  // function types are otherwise the same -> 'pointer to member of C2 of type
  // function' or 'pointer to member of C1 of type function', respectively.
  test<int (A::*)() noexcept, int (A::*)(), int (A::*)()>();
  test<int (A::*)(), int (A::*)() noexcept, int (A::*)()>();
}
