/* { dg-do run } */
/* { dg-options "-O2" } */

int q, sink;

int __attribute__ ((noipa))
flag_clobbers_cc (int a, int b, int *p)
{
  int s = a | b;

  *p = s;
  __builtin_arc_flag (0);
  if (s == 0)
    q = 1;

  return s;
}

#if defined (__ARCEM__) || defined (__ARCHS__)
int r;

int __attribute__ ((noipa))
kflag_clobbers_cc (int a, int b, int *p)
{
  int s = a | b;

  *p = s;
  __builtin_arc_kflag (0);
  if (s == 0)
    r = 1;

  return s;
}
#endif

int
main (void)
{
  flag_clobbers_cc (0, 0, &sink);
  if (q != 1)
    __builtin_abort ();

#if defined (__ARCEM__) || defined (__ARCHS__)
  kflag_clobbers_cc (0, 0, &sink);
  if (r != 1)
    __builtin_abort ();
#endif

  return 0;
}
