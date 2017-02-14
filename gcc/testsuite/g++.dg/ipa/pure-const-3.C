/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-vrp -fdump-tree-optimized"  } */
int *ptr;
static int barvar;
static int b(int a);
/* We can not detect A to be const because it may be interposed by unoptimized
   body.  */
inline
__attribute__ ((noinline))
int a(int a)
{
  if (a>0)
    return b(a-1);
  return *ptr == *ptr;
}
inline
__attribute__ ((noinline))
static int b(int p)
{
  if (p<0)
    return a(p+1);
  return 1;
}
main()
{
  int aa;
  ptr = &barvar;
  aa=!b(3);
  ptr = 0;
  return aa;
}
/* { dg-final { scan-tree-dump "barvar"  "optimized"  } } */
