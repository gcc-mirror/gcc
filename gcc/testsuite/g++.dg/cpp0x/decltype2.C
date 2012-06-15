// { dg-do compile }
// { dg-options "-std=gnu++0x" }

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
  static_assert(is_same< DECLTYPE , RESULT >::value, #RESULT)

struct A {};

int a; 
int& b = a; 
const int& c = a; 
const int d = 5; 
const A e = A(); 
CHECK_DECLTYPE(decltype(a), int);
CHECK_DECLTYPE(decltype(b), int&); 
CHECK_DECLTYPE(decltype(c), const int&); 
CHECK_DECLTYPE(decltype(d), const int); 
CHECK_DECLTYPE(decltype(e), const A); 

CHECK_DECLTYPE(decltype(a), int);
CHECK_DECLTYPE(decltype((a)), int&);

void foo_check(int a, int& b, float& c, int* d) 
{ 
  CHECK_DECLTYPE(decltype(a), int);
  CHECK_DECLTYPE(decltype(b), int&); 
  CHECK_DECLTYPE(decltype(c), float&);
  CHECK_DECLTYPE(decltype(d), int*);
} 

int foo(char); 
int bar(char); 
int bar(int); 
CHECK_DECLTYPE(decltype(foo), int(char));

decltype(bar) z; // { dg-error "overload" "overload" }
// { dg-error "invalid type" "invalid" { target *-*-* } 48 }

CHECK_DECLTYPE(decltype(&foo), int(*)(char));
CHECK_DECLTYPE(decltype(*&foo), int(&)(char));

void array_types()
{
  int a[10]; 
  CHECK_DECLTYPE(decltype(a), int[10]);
}

