/* { dg-require-effective-target label_values } */

/* As a quality of implementation issue, we should not prevent inlining
   of function explicitly marked inline just because a label therein had
   its address taken.  */

static void *ptr1, *ptr2;
static int i = 1;

static __inline__ void doit(void **pptr, int cond)
{
  if (cond) {
  here:
    *pptr = &&here;
  }
}

__attribute__ ((noinline))
static void f(int cond)
{
  doit (&ptr1, cond);
}

__attribute__ ((noinline))
static void g(int cond)
{
  doit (&ptr2, cond);
}

__attribute__ ((noinline))
static void bar(void);

int main()
{
  f (i);
  bar();
  g (i);

#ifdef  __OPTIMIZE__
  if (ptr1 == ptr2)
    abort ();
#endif

  exit (0);
}

void bar(void) { }
