/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ccp1"  } */
struct A
 {
   int a;
   virtual int bar(void) {return a;}
 };
struct B
 {
   virtual int foo(void) {return b;}
   int b;
 };
struct C: A,B
 {
   virtual int foo(void) {return a;}
 };

struct C c;
int test(void)
{
  struct C *d=&c;
  struct B *b=d;
  return d->foo()+b->foo();
}
/* The call to b->foo() is perfectly devirtualizable because C can not be in construction
   when &c was used.  */
/* { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "ccp1"  } } */
