/* Regression test for ICE seen handling callbacks.  */

int
middle (int flag,
	int (*on_success) (),
	int (*on_failure) ())
{
  if (flag)
    return on_success ();
  else
    return on_failure ();
}

static int
success_callback ()
{
  return 0;
}

static int
failure_callback_x ()
{
  return -1;
}

static int
failure_callback_y ()
{
  return -1;
}

int
outer (int flag_a, int flag_b)
{
  return middle (flag_a, success_callback,
		 (flag_b
		  ? failure_callback_x
		  : failure_callback_y));
}
