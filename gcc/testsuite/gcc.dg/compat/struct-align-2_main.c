/* Test compatibility of structure layout and alignment for a struct
   containing an int and a long long, with various combinations of
   packed and aligned attributes.  The struct is from the Linux kernel.  */

extern void struct_align_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_align_2_x ();
  exit (0);
}
