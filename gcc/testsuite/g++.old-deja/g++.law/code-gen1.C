// { dg-do run  }
// GROUPS passed code-generation
// code-gen file
// From: Jeffrey C. Gealow <jgealow@mtl.mit.edu>
// Date:     Sun, 4 Jul 93 18:57:53 -0400
// Subject:  increment bug (0 + 1 + 1 = 3)
// Message-ID: <9307042257.AA23538@mtl.mit.edu>

#include <stdio.h>

int main()
{
  int i = 0;
  (++i)++;
  if (i == 2)
    printf ("PASS\n");
  else
    { printf ("FAIL\n"); return 1; }
}


