/* Test variable sized function argument passing.
   GCC 3.2 and earlier is incompatible with GCC 3.3+ on x86-64,
   the latter passes variable sized arguments by reference while
   the former doesn't.
   See http://gcc.gnu.org/ml/gcc-patches/2003-01/msg01830.html */

extern void struct_by_value_22_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_22_x ();
  exit (0);
}
