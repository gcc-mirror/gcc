/* As a quality of implementation issue, we should not prevent inlining
   of function explicitly marked inline just because a label therein had
   its address taken.  */

#ifndef NO_LABEL_VALUES
static void *ptr1, *ptr2;
static int i = 1;

static __inline__ void doit(void **pptr, int cond)
{
  if (cond) {
  here:
    *pptr = &&here;
  }
}

static void f(int cond)
{
  doit (&ptr1, cond);
}

static void g(int cond)
{
  doit (&ptr2, cond);
}

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

#else /* NO_LABEL_VALUES */
int main() { exit(0); }
#endif
