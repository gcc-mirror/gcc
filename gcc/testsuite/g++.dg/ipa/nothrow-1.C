/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -fdump-tree-optimized"  } */
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
main()
{
  int aa;
  ptr = &barvar;
  try {
    aa=!a();
  } catch  (...)
  {
    return 1;
  }
  ptr = 0;
  return aa;
}
/* { dg-final { scan-tree-dump "__cxa_begin_catch"  "optimized"  } } */
