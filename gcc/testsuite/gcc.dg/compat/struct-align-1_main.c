/* Test compatibility of structure layout and alignment for structs
   which contain doubles.  The original structs here are from PR 10645.  */

extern void struct_align_1_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_align_1_x ();
  exit (0);
}
