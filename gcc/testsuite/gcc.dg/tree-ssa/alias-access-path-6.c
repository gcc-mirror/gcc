/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* This tests that nonoveralpping_component_refs does not give up
   on field delcs and continues looking to find mismatch between
   a1 and a2.  */
struct a {
	int a:3;
	int b:3;
};
struct b {struct a a1,a2;};
struct c {struct b b[10];} *cptr;
struct d {struct c c;} *dptr;
int
test(int i,int j)
{
  cptr->b[i].a1.a=0;
  dptr->c.b[j].a2.b=1;
  return cptr->b[i].a1.a;
}
int
test2(int i,int j)
{
  cptr->b[i].a1.a=1;
  dptr->c.b[j].a1.a=0;
  return cptr->b[i].a1.a;
}
/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-not "return 1" "optimized"} } */
