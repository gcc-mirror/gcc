/* PR c/55976 */
/* { dg-do compile } */
/* { dg-options "-Wno-return-mismatch" } */

/* Verify that -Wno-return-mismatch turns off warnings about function return
   type.  */

void t () { return 1; } /* normally generates function returning void */
int b () { return; } /* normally generates function returning non-void */

int main()
{
  t (); b ();
  return 0;
}

