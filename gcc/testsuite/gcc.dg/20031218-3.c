/* Orgin: Chris Demetriou <cgd@broadcom.com>
   PR debug/12923  ICE in gen_subprogram_die with -O2 -g
   The problem was that this just to ICE with -O2 -g.  */

/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

extern __SIZE_TYPE__ strlen (const char *);

int x (char *s)
{
  int y () { return (strlen (s)); }
  return y (s);
}
