// ehopt was only copying one statement from the cleanup of the B temporary
// into the following try block, so we lost its destructor call.

// { dg-do run }

template <class T, class U>
class A;

bool b;
int count;

template <>
class A<int, int>
{
public:
  A(int) { ++count; if (b) throw 1; }
  A(const A&) { ++count; if (b) throw 1; }
  ~A() { --count; if (b) throw 1; }
};

typedef A<int, int> B;

template <>
class A<void *, void *>
{
public:
  A() { if (b) throw 1; }
  A(const B&) { if (b) throw 1; }
  ~A() { if (b) throw 1; }
};

typedef A<void *, void *> C;

void f() { if (b) throw 1; }

int
main (void)
{
  {
    C a(1);
    f();
  }
  return count;
}
