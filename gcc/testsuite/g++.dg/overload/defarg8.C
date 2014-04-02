// PR c++/60367
// { dg-do run { target c++11 } }

extern "C" int printf (const char *, ...);
extern "C" void abort();

void *p;
struct foo {
  foo() { p = this; }
  foo (const foo &) { abort(); }
  ~foo() { if (p != this) abort(); }
};

void do_something( foo f = {} )
{
  if (&f != p) abort();
}

int main()
{
 do_something();
}
