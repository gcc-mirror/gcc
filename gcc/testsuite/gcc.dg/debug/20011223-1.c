/* Origin: PR c/5163 from aj@suse.de.  */
/* { dg-do compile } */

extern int bar (int);

int
foo (void)
{
  extern int bar (int);
  return bar (5);
}
