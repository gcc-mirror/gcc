/* { dg-require-effective-target indirect_calls } */

/* Test function call with function designator involving VLA
   side-effects does not lead to an ICE.  */

void f (void);
void g (void);

void
h (int a, void *b)
{
  ((void *)(int (*)[++a])b ? f : g) ();
}
