// PR c++/45487
// { dg-do run  }

extern "C" int strcmp(const char*, const char*);

template <typename T>
const char* foo(T, typename T::type c) { return __PRETTY_FUNCTION__; }

struct x {typedef int type;};

int main()
{
  if (strcmp (foo(x(), 3), "const char* foo(T, typename T::type) "
	      "[with T = x; typename T::type = int]") == 0)
    return 0;
  else 
    return 1;
}
