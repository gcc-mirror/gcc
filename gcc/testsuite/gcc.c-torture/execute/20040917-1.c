/* submitted by kenneth zadeck */

static int test_var;

/* the idea here is that not only is inlinable, inlinable but since it
   is static, the cgraph node will not be marked as output.  The
   current version of the code ignores these cgraph nodes.  */

void not_inlinable()  __attribute__((noinline));

static void  
inlinable ()
{
  test_var = -10;
}

void 
not_inlinable ()
{
  inlinable();
}

main ()
{
  test_var = 10;
  /* Variable test_var should be considered call-clobbered by the call
     to not_inlinable().  */
  not_inlinable ();
  if (test_var == 10)
    abort ();
  return 0;
}
