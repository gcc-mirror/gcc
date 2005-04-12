/* This used to ICE due to a regmove problem on s390.  */

/* { dg-do compile { target s390*-*-* } } */
/* { dg-options "-O2" } */


extern void abort (void);
extern void **alloc (void);

void *test (void)
{
  void **p = alloc ();
  if (!p) abort ();

  __builtin_set_thread_pointer (p);
  return *p;
}

