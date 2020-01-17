// { dg-do compile }

void _setjmp(void *);
struct S { ~S(); };
void * (* fn)();
void f();
void g()
{
  S s;
  _setjmp(fn());
  []{ f(); }();
}
