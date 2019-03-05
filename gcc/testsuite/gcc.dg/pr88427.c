/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce -fno-tree-fre -Wno-div-by-zero" } */

void
uj (int eq, int s4)
{
  short int tm = 0;

  for (;;)
    if (eq == s4)
      {
	tm += !!s4;
	if (tm == s4)
	  {
	    eq += tm;
	    for (;;)
	      eq /= 0;
	  }
      }
}
