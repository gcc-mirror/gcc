// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: 

void *vp;

void f ()
{
  int i = (*((int (*)[i]) vp))[0];
}
