// PR c++/106784
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

template<typename From, typename To>
struct is_convertible {
  static const bool value = __is_convertible(From, To);
};

struct from_int {
  from_int(int);
};

struct from_charp {
  from_charp(const char *);
};

struct to_int {
  operator int();
};

typedef int Fn(int);
typedef char Arr[10];
enum E { XYZZY };

SA(!__is_convertible(int, void));
SA(__is_convertible(int, int));
SA(__is_convertible(int, from_int));
SA(__is_convertible(long, from_int));
SA(__is_convertible(double, from_int));
SA(__is_convertible(const int, from_int));
SA(__is_convertible(const int&, from_int));
SA(__is_convertible(to_int, int));
SA(__is_convertible(to_int, const int&));
SA(__is_convertible(to_int, long));
SA(!__is_convertible(to_int, int&));
SA(!__is_convertible(to_int, from_int));
SA(!__is_convertible(int, Fn));
SA(!__is_convertible(int, Fn*));
SA(!__is_convertible(int, Fn&));
SA(!__is_convertible(int, Arr));
SA(!__is_convertible(int, Arr&));
SA(!__is_convertible(int, int&));
SA(__is_convertible(int, const int&));
SA(!__is_convertible(const int, int&));
SA(__is_convertible(const int, const int&));
SA(!__is_convertible(int, int*));

SA(!__is_convertible(int, E));
SA(__is_convertible(E, int));

SA(__is_convertible(int&, int));
SA(__is_convertible(int&, int&));
SA(__is_convertible(int&, const int&));
SA(!__is_convertible(const int&, int&));
SA(__is_convertible(const int&, const int&));
SA(!__is_convertible(int&, int*));
SA(!__is_convertible(int&, void));
SA(!__is_convertible(int&, Fn));
SA(!__is_convertible(int&, Fn*));
SA(!__is_convertible(int&, Fn&));
SA(!__is_convertible(int&, Arr));
SA(!__is_convertible(int&, Arr&));

SA(!__is_convertible(int*, int));
SA(!__is_convertible(int*, int&));
SA(!__is_convertible(int*, void));
SA(__is_convertible(int*, int*));
SA(__is_convertible(int*, const int*));
SA(!__is_convertible(const int*, int*));
SA(__is_convertible(const int*, const int*));
SA(!__is_convertible(int*, Fn));
SA(!__is_convertible(int*, Fn*));
SA(!__is_convertible(int*, Fn&));
SA(!__is_convertible(int*, Arr));
SA(!__is_convertible(int*, Arr&));
SA(!__is_convertible(int*, float*));

SA(__is_convertible(void, void));
SA(!__is_convertible(void, char));
SA(!__is_convertible(void, char&));
SA(!__is_convertible(void, char*));
SA(!__is_convertible(char, void));
SA(__is_convertible(const void, void));
SA(__is_convertible(void, const void));
SA(__is_convertible(const void, const void));
SA(!__is_convertible(void, Fn));
SA(!__is_convertible(void, Fn&));
SA(!__is_convertible(void, Fn*));
SA(!__is_convertible(void, Arr));
SA(!__is_convertible(void, Arr&));

SA(!__is_convertible(Fn, void));
SA(!__is_convertible(Fn, Fn));
SA(__is_convertible(Fn, Fn*));
SA(__is_convertible(Fn, Fn&));
SA(!__is_convertible(int(int), int(int)));
SA(__is_convertible(int(int), int(&)(int)));
SA(__is_convertible(int(int), int(&&)(int)));
SA(__is_convertible(int(int), int(*)(int)));
SA(__is_convertible(int(int), int(*const)(int)));
SA(!__is_convertible(int(int), char));
SA(!__is_convertible(int(int), char*));
SA(!__is_convertible(int(int), char&));

SA(!__is_convertible(Fn&, void));
SA(!__is_convertible(Fn&, Fn));
SA(__is_convertible(Fn&, Fn&));
SA(__is_convertible(Fn&, Fn*));
SA(!__is_convertible(Fn&, Arr));
SA(!__is_convertible(Fn&, Arr&));
SA(!__is_convertible(Fn&, char));
SA(!__is_convertible(Fn&, char&));
SA(!__is_convertible(Fn&, char*));

SA(!__is_convertible(Fn*, void));
SA(!__is_convertible(Fn*, Fn));
SA(!__is_convertible(Fn*, Fn&));
SA(__is_convertible(Fn*, Fn*));
SA(!__is_convertible(Fn*, Arr));
SA(!__is_convertible(Fn*, Arr&));
SA(!__is_convertible(Fn*, char));
SA(!__is_convertible(Fn*, char&));
SA(!__is_convertible(Fn*, char*));

SA(!__is_convertible(Arr, void));
SA(!__is_convertible(Arr, Fn));
SA(!__is_convertible(Arr, Fn*));
SA(!__is_convertible(Arr, Fn&));
SA(!__is_convertible(Arr, Arr));
SA(!__is_convertible(Arr, Arr&));
SA(__is_convertible(Arr, const Arr&));
SA(!__is_convertible(Arr, volatile Arr&));
SA(!__is_convertible(Arr, const volatile Arr&));
SA(!__is_convertible(const Arr, Arr&));
SA(__is_convertible(const Arr, const Arr&));
SA(__is_convertible(Arr, Arr&&));
SA(__is_convertible(Arr, const Arr&&));
SA(__is_convertible(Arr, volatile Arr&&));
SA(__is_convertible(Arr, const volatile Arr&&));
SA(__is_convertible(const Arr, const Arr&&));
SA(!__is_convertible(Arr&, Arr&&));
SA(!__is_convertible(Arr&&, Arr&));
SA(!__is_convertible(Arr, char));
SA(__is_convertible(Arr, char*));
SA(__is_convertible(Arr, const char*));
SA(!__is_convertible(Arr, char&));
SA(!__is_convertible(const Arr, char*));
SA(__is_convertible(const Arr, const char*));
SA(!__is_convertible(int, int[1]));
SA(!__is_convertible(int[1], int[1]));
SA(!__is_convertible(int[1], int(&)[1]));
SA(__is_convertible(int(&)[1], int(&)[1]));
SA(__is_convertible(int(&)[1], const int(&)[1]));
SA(!__is_convertible(const int(&)[1], int(&)[1]));
SA(!__is_convertible(int[1][1], int*));
SA(!__is_convertible(int[][1], int*));

SA(!__is_convertible(Arr&, void));
SA(!__is_convertible(Arr&, Fn));
SA(!__is_convertible(Arr&, Fn*));
SA(!__is_convertible(Arr&, Fn&));
SA(!__is_convertible(Arr&, Arr));
SA(__is_convertible(Arr&, Arr&));
SA(__is_convertible(Arr&, const Arr&));
SA(!__is_convertible(const Arr&, Arr&));
SA(__is_convertible(const Arr&, const Arr&));
SA(!__is_convertible(Arr&, char));
SA(__is_convertible(Arr&, char*));
SA(__is_convertible(Arr&, const char*));
SA(!__is_convertible(Arr&, char&));
SA(!__is_convertible(const Arr&, char*));
SA(__is_convertible(const Arr&, const char*));
SA(__is_convertible(Arr, from_charp));
SA(__is_convertible(Arr&, from_charp));

struct B { };
struct D : B { };

SA(__is_convertible(D, B));
SA(__is_convertible(D*, B*));
SA(__is_convertible(D&, B&));
SA(!__is_convertible(B, D));
SA(!__is_convertible(B*, D*));
SA(!__is_convertible(B&, D&));

/* These are taken from LLVM's test/SemaCXX/type-traits.cpp.  */

struct I {
  int i;
  I(int _i) : i(_i) { }
  operator int() const {
    return i;
  }
};

struct F
{
  float f;
  F(float _f) : f(_f) {}
  F(const I& obj)
    : f(static_cast<float>(obj.i)) {}
  operator float() const {
    return f;
  }
  operator I() const {
    return I(static_cast<int>(f));
  }
};

SA(__is_convertible(I, I));
SA(__is_convertible(I, const I));
SA(__is_convertible(I, int));
SA(__is_convertible(int, I));
SA(__is_convertible(I, F));
SA(__is_convertible(F, I));
SA(__is_convertible(F, float));
SA(__is_convertible(float, F));

template<typename>
struct X {
  template<typename U> X(const X<U>&);
};

SA(__is_convertible(X<int>, X<float>));
SA(__is_convertible(X<float>, X<int>));

struct Abstract {
  virtual void f() = 0;
};

SA(!__is_convertible(Abstract, Abstract));

class hidden {
  hidden(const hidden&);
  friend void test ();
};

SA(__is_convertible(hidden&, hidden&));
SA(__is_convertible(hidden&, const hidden&));
SA(__is_convertible(hidden&, volatile hidden&));
SA(__is_convertible(hidden&, const volatile hidden&));
SA(__is_convertible(const hidden&, const hidden&));
SA(__is_convertible(const hidden&, const volatile hidden&));
SA(__is_convertible(volatile hidden&, const volatile hidden&));
SA(__is_convertible(const volatile hidden&, const volatile hidden&));
SA(!__is_convertible(const hidden&, hidden&));

void
test ()
{
  /* __is_convertible(hidden, hidden) should be false despite the
     friend declaration above, because "Access checks are performed
     as if from a context unrelated to either type", but we don't
     implement that for the built-in (std::is_convertible works as
     expected).  This is the case for __is_assignable as well.  */
  //SA(!__is_convertible(hidden, hidden));
}

void
test2 ()
{
  struct X { };
  struct Y {
    explicit Y(X); // not viable for implicit conversions
  };
  SA(!__is_convertible(X, Y));
}
