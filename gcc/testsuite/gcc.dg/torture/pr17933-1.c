/* PR rtl-optimization/17933.
   Test-case from the ObjC test-suite, execute/class_self-2.m,
   translated to C from and reduced by Andrew Pinski.  */

struct d
{ int a; };
void abort(void);
typedef struct d (*f) (int i);
f ff(void);
void test1()
{
  f t = ff();
  t(0);
}
