/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-devirt"  } */
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
   when &c was used, but we can not analyze that so far.  Test that we at least speculate
   that type is in the construction.  */
/* { dg-final { scan-ipa-dump "Speculatively devirtualizing" "devirt"  } } */
/* { dg-final { cleanup-ipa-dump "devirt" } } */
