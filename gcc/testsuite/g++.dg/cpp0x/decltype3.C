// { dg-do compile { target c++11 } }

template<typename T, typename U> 
struct is_same 
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

#define CHECK_DECLTYPE(DECLTYPE,RESULT) \
  static_assert(is_same< DECLTYPE , RESULT >::value, #DECLTYPE " should be " #RESULT)

class A { 
public:
  int a; 
  int& b; 
  static int c; 

  A(int& b) : b(b) { }

  void foo() { 
    CHECK_DECLTYPE(decltype(a), int);
    CHECK_DECLTYPE(decltype(this->a), int);
    CHECK_DECLTYPE(decltype((*this).a), int);
    CHECK_DECLTYPE(decltype(b), int&);
    CHECK_DECLTYPE(decltype(c), int);
  } 
  void bar() const {
    CHECK_DECLTYPE(decltype(a), int);
    CHECK_DECLTYPE(decltype(b), int&);
    CHECK_DECLTYPE(decltype(c), int);
  } 
}; 

int b;
A aa(b); 
const A& caa = aa; 
CHECK_DECLTYPE(decltype(aa.a), int);
CHECK_DECLTYPE(decltype(aa.b), int&);
CHECK_DECLTYPE(decltype(caa.a), int);

class B { 
public:
  int a;
  enum B_enum { b }; 
  decltype(a) c;
  decltype(a) foo() { return 0; }
  decltype(b) enums_are_in_scope() { return b; } // ok 
}; 

CHECK_DECLTYPE(decltype(aa.*&A::a), int&);
decltype(aa.*&A::b) zz; // { dg-error "cannot create pointer to reference member" "cannot" }

CHECK_DECLTYPE(decltype(caa.*&A::a), const int&);

class X { 
  void foo() { 
    CHECK_DECLTYPE(decltype(this), X*);
    CHECK_DECLTYPE(decltype(*this), X&);
  } 
  void bar() const { 
    CHECK_DECLTYPE(decltype(this), const X*);
    CHECK_DECLTYPE(decltype(*this), const X&);
  } 
};

