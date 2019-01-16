/* { dg-require-effective-target alloca } */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */

/* Test variable sized function argument passing.
   GCC 3.2 and earlier is incompatible with GCC 3.3+ on x86-64,
   the latter passes variable sized arguments by reference while
   the former doesn't.
   See http://gcc.gnu.org/ml/gcc-patches/2003-01/msg01830.html */

#ifndef SKIP_VLA_IN_STRUCT
extern void struct_by_value_22_x (void);
#endif
extern void exit (int);
int fails;

int
main ()
{
#ifndef SKIP_VLA_IN_STRUCT
  struct_by_value_22_x ();
#endif
  exit (0);
}
