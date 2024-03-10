#ifndef FLOAT_POINT_FRM_RUN_H
#define FLOAT_POINT_FRM_RUN_H

static void
assert_equal (int a, int b, char *message)
{
  if (a != b)
    {
      printf ("%s, but get %d != %d\n", message, a, b);
      __builtin_abort ();
    }
}

static int
get_frm ()
{
  int frm = -1;

  __asm__ volatile (
    "frrm %0"
    :"=r"(frm)
    :
    :
  );

  return frm;
}

static void
set_frm (int frm)
{
  __asm__ volatile (
    "fsrm %0"
    :
    :"r"(frm)
    :
  );
}

#endif
