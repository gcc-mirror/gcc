// GROUPS passed initialization
// global-init file
// Message-Id: <9212021756.AA12639@grumpy.pocs.com>
// From: wp@pocs.com (Wolfgang Polak)
// Subject: Initializers - gcc 2.2.2 (g++), SPARC, SunOS 4.1.1
// Date: Wed, 2 Dec 92 09:56:01 PST

#include <stdio.h>
struct S { int  a; int  c; };
int i = 3;
S s = {6, i};
S * the_s  () { return &s; };
int main ()
{
  S * cls = the_s ();
  if (cls->a != 6)
    { printf ("FAIL\n"); return 1; }
  else
    printf ("PASS\n");
}
