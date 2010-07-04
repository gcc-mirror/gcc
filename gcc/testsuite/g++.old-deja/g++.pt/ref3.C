// { dg-do run  }
// { dg-prune-output "mangled name" }
// Origin: John Wilkinson <jfw@sgi.com>

template <class T, int& Size> 
struct Base {
    Base() : obj(Size) {}
    T obj;
};

int globalInt = 5;

struct A {
    A(int arg) : ia(arg) {}
    int ia;
};

int main()
{
  Base<A, globalInt> ob;
  if (ob.obj.ia != 5)
    return 1;
}
