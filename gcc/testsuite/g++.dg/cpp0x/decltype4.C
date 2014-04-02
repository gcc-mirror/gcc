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

struct A {
  int x; 
  int& y; 
  int foo(char); 
  int& bar() const; 
}; 

CHECK_DECLTYPE(decltype(&A::x), int A::*);
decltype(&A::y) Ay; // { dg-error "cannot create pointer to reference member|invalid type" }
CHECK_DECLTYPE(decltype(&A::foo), int (A::*) (char));
CHECK_DECLTYPE(decltype(&A::bar), int& (A::*) () const);

CHECK_DECLTYPE(decltype("decltype"), const char(&)[9]);
CHECK_DECLTYPE(decltype(1), int);

int an_int = 5;
int& i = an_int; 
const int j = an_int; 

CHECK_DECLTYPE(decltype(i)&, int&);
CHECK_DECLTYPE(const decltype(j), const int);

int foo(); 
CHECK_DECLTYPE(decltype(foo()), int);
float& bar(int); 
CHECK_DECLTYPE(decltype (bar(1)), float&);
const A bar(); 
CHECK_DECLTYPE(decltype (bar()), const A);
const A& bar2(); 
CHECK_DECLTYPE(decltype (bar2()), const A&);

void wibble() {
  CHECK_DECLTYPE(decltype(1+2), int);
  int* p; 
  CHECK_DECLTYPE(decltype(*p), int&);
  int a[10]; 
  CHECK_DECLTYPE(decltype(a[3]), int&);
  int i; int& j = i; 
  CHECK_DECLTYPE(decltype (i = 5), int&);
  CHECK_DECLTYPE(decltype (j = 5), int&);

  CHECK_DECLTYPE(decltype (++i), int&); 
  CHECK_DECLTYPE(decltype (i++), int);
}

struct B {
  B () : bit(), cbit() {} 
  int bit : 2;
  const int cbit : 3;

  void foo()
  {
    CHECK_DECLTYPE(decltype(bit), int);
    CHECK_DECLTYPE(decltype((bit)), int&);
    CHECK_DECLTYPE(decltype(cbit), const int);
    CHECK_DECLTYPE(decltype((cbit)), const int&);
  }
};

B b;
const B& bc = b;
CHECK_DECLTYPE(decltype(b.bit), int);
CHECK_DECLTYPE(decltype(bc.bit), int);
CHECK_DECLTYPE(decltype((b.bit)), int&);
CHECK_DECLTYPE(decltype((bc.bit)), const int&);
