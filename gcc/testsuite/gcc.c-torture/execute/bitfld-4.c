/* When comparisons of bit-fields to unsigned constants got shortened,
   the shortened signed constant was wrongly marked as overflowing,
   leading to a later integer_zerop failure and misoptimization.

   Related to bug tree-optimization/16437 but shows the problem on
   32-bit systems.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */

extern void abort (void);

struct s { int a:12, b:20; };

struct s x = { -123, -456 };

int
main (void)
{
  if (x.a != -123U || x.b != -456U)
    abort ();
  return 0;
}
