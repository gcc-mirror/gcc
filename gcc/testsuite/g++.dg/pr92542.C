template<typename T>
struct S
{
  S(T, T) { }
};

char* begin();
char* end();

void
test01()
{
  S s(begin(). end());  // { dg-error "request for member" "" { target c++17 } }
  // { dg-error "missing" "" { target c++14_down } .-1 }
}
