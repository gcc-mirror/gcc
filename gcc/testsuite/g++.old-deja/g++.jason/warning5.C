// PRMS Id: 5135
// Bug: g++ complains that the result of the new expression is not used.
// Special g++ Options: -Wall

extern "C" int printf (const char *, ...);
inline void * operator new (__SIZE_TYPE__, void *p) { return p; }

class foo {
public:
  foo() : a(42) {};
  int a;
};

int
main()
{
  char buffer[1024];

  new (buffer) foo;

  foo* pY = (foo *)buffer;

  return pY->a != 42;
}
