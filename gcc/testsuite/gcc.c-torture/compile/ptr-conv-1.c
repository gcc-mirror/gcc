/* The intermediate conversion to __PTRDIFF_TYPE__ could be lost,
   resulting in an "invalid types in nop conversion" ICE.  */
long long a;
void
f (void)
{
  int c = 1;
  volatile int *p = &c;
  a = (long long) (__PTRDIFF_TYPE__) p;
  *p;
}
