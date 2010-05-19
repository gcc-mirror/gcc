void abort (void);
extern int *a[5];
extern int b,c,d,e,f;
__attribute__ ((noinline, noclone))
void
other_ltrans (void)
{
  if (!__builtin_constant_p (a[1]==a[2]))
    abort ();
  if (a[1]!=a[2])
    abort ();
  *(a[1])=11;
}
