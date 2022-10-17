/* PR c/102759 - ICE calling a function taking an argument redeclared
   without a prototype.
   { dg-do compile }
   { dg-options "-Wall" } */

void f (void)
{
  void gia (int[2]);
  void g ();
}

/* Redeclaring the g(int[]) above without a prototype loses it.  */
void gia ();
void g (int[2]);

void h (void )
{
  gia (gia);
  gia (g);
}
