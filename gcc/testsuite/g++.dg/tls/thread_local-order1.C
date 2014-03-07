// { dg-do run { target c++11 } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }
// { dg-require-cxa-atexit "" }

extern "C" void abort();
extern "C" int printf (const char *, ...);
#define printf(...)

int c;
struct A {
  int i;
  A(int i): i(i) { printf ("A(%d)\n", i); if (i != c++) abort (); }
  ~A() { printf("~A(%d)\n", i); if (i != --c) abort(); }
};

A a0(0);
thread_local A a1(1);
thread_local A a2(2);
A* ap = &a1;

int main()
{
  if (c != 3) abort();
}
