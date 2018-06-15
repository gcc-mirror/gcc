// PR c++/82282
// { dg-do compile { target c++14 } }

template<typename = int>
void f(const char* a =
       ([](int = []{ static int i; return 42; }()) {
	  static int i;
	  return "";
	}()));

template<typename = int>
struct X {
  void f(const char* a =
	 ([](int = [] { static int i; return 42; }()) { 
	    enum { Size = 42 - 1 };
	    return "";
	  }()));
};

void g()
{
  f();
  X<int>().f();
}
