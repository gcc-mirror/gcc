extern void main_test (void);
int inside_main;

int
main ()
{
  inside_main = 1;
  main_test ();
  inside_main = 0;
  return 0;
}

/* When optimizing, all the constant cases should have been
   constant folded, so no calls to link_error should remain.
   In any case, link_error should not be called.  */

#ifndef __OPTIMIZE__
void
link_error (void)
{
  abort ();
}
#endif
