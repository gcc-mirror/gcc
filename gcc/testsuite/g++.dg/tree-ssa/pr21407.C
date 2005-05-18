/* { dg-do run } */
/* { dg-options "-O2" } */
extern "C" void abort(void);
struct T1 {int a, b; virtual void f(){}};
struct T : T1 { struct T1 w;  int b; };
void foo (struct T1 *p) { struct T *q = dynamic_cast<T*>(p); if (q->b != 2) abort (); }
/* We shouldn't kill the store to c.b, because foo uses it.  */
int main () { struct T c; c.b = 2; foo (&c); return 0; }
