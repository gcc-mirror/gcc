/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre -fdump-tree-evrp" } */

struct A{int a,b;};
inline int*f1(struct A*p){return&p->a;}   /* offset of 0.  */
inline int*f2(struct A*p){return&p->b;}   /* Offset of non-zero.  */
inline int*g(struct A*p){return(int*)p+1;} /* Always non-zero offet.  */

/* Should be able to eliminate all calls to bad().  */
 
void bad(void);

int
main() 
{
  struct A* ptr = 0;
  struct A addr;

  if (f1 (ptr) != 0)
    bad();
  if (f1 (&addr) == 0)
    bad();

  if (f2 (ptr) == 0)
    bad();
  if (f2 (&addr) == 0)
    bad();
    
  if (g (ptr) == 0)
    bad();
  if (g (&addr) == 0)
    bad();
  
}

/* { dg-final { scan-tree-dump-not "bad" "evrp"} } */

