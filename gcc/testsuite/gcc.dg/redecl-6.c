/* Test for multiple declarations and composite types.  As in bug
   13801.  Illustrates how bug causes correct code to be wrongly
   diagnosed.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int IA[];
typedef int A5[5];
typedef int A10[10];

A10 array10;

A5 *ap;
void
f (void)
{
  int ap;
  {
    extern IA *ap;
    /* This assignment is valid.  */
    ap = &array10;
  }
}
