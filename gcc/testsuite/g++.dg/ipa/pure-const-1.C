/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
int *ptr;
static int barvar;

/* We can not detect A to be const because it may be interposed by unoptimized
   body.  */
inline
__attribute__ ((noinline))
int a(void)
{
  return *ptr == *ptr;
}
int main()
{
  int aa;
  ptr = &barvar;
  aa=!a();
  ptr = 0;
  return aa;
}
/* { dg-final { scan-tree-dump "barvar"  "optimized"  } } */
