/* Orgin: Richard Sandiford <rsandifo@gcc.gnu.org> 
   PR debug/12923  ICE in gen_subprogram_die with -O2 -g
   The problem was that this just to ICE with -O2 -g.  */

/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int f1 (int y)
{
  int f2() { return y; }
  return f2();
}
