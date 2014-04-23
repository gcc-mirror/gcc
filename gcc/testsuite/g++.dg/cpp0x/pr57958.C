// { dg-do run { target c++11 } }

#define assert(E) if(!(E))__builtin_abort();

int n = 0;

template <class T>
class Foo {
 public:
  Foo() { 
   n--; 
  }
  Foo(const Foo&) { 
   n--; 
  }
  ~Foo() { 
   n++; 
  }
};

struct Data {};

void a()
{
	Data b;
}

int main(int argc, char *argv[]) {
  auto fn = [] (const Foo<Data>& x) {
    return (x);
  };

  {
    Foo<Data> a;
    fn(a);
  }

  assert(n == 0);
}
