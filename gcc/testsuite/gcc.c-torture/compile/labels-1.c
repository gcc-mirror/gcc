#ifndef NO_LABEL_VALUES
f ()
{
  void *x = &&L2;
  if (&&L3 - &&L1 > 1)
    abort();
 L1: return 1;
 L2: abort ();
 L3:;
}
#else
int x;
#endif
