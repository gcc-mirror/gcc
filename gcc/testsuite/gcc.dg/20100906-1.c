/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -fno-short-enums -Wl,--no-enum-size-warning" {target arm_eabi} } */

/* This testcase got misoptimized by combine due to a wrong setting of
   subst_low_luid in try_combine.  */

enum rtx_code {
  A, B
};

void abort (void);

struct rtx_def {
  __extension__ enum rtx_code code:16;
};
typedef struct rtx_def *rtx;

void __attribute__((noinline))
add_constraint (unsigned char is_a)
{
  if (is_a)
    abort ();
}

void __attribute__((noinline))
foo (rtx defn)
{
  switch (defn->code)
    {
    case A:
    case B:
      add_constraint (defn->code == A);
      break;
    default:
      break;
    }
}

int
main ()
{
  struct rtx_def r;

  r.code = B;

  foo (&r);
  return 0;
}
