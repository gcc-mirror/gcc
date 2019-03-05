/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fno-ipa-reference-addressable" } */
static struct a {int magic1,b;} a;
volatile int magic2;
static struct b {int a,b,c,d,e,f;} magic3;

struct b foo();

void
t()
{
 a.magic1 = 1;
 magic2 = 1;
 magic3 = foo();
}
/* { dg-final { scan-tree-dump "magic1" "optimized"} } */
/* { dg-final { scan-tree-dump "magic3" "optimized"} } */
/* { dg-final { scan-tree-dump "magic2" "optimized"} } */
/* { dg-final { scan-tree-dump "foo" "optimized"} } */
 
