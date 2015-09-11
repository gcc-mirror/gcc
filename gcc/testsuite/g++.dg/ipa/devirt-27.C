/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized"  } */
struct A
 {
   int a;
 };
struct B
 {
  __attribute__ ((visibility("default")))
   virtual int foo(void) {return 42;}
   int b;
 };
struct C: A,B
 {
  __attribute__ ((visibility("hidden")))
   virtual int foo(void);
 };

struct C c;
int test(void)
{
  struct C *d=&c;
  struct B *b=d;
  return d->foo()+b->foo();
}
/* { dg-final { scan-tree-dump "OBJ_TYPE_REF" "optimized"  } } */
