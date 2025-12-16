/* { dg-do run } */
/* { dg-require-effective-target memtag_exec } */
/* { dg-additional-options "-O2" } */

#include <string.h>
#include "mte-sig.h"

void
bar (int i, char *b)
{
  __builtin_printf ("i = %d, b+i = %s \n", i, b + i);
}

void
foo (int size)
{
  int i;
  for (i = 0; i < size; i++)
    {
      char temp[size];
      memset (temp, 'B', size);
      temp[size-1] = '\0';
      {
	char temp2[size];
	memset (temp2, 'A', size);
	temp2[size-1] = '\0';
	bar (i, temp2);
      }
      bar (i, temp);
    }
}

int main (void)
{
  setHandler ();
  foo (10);
  foo (3);
  return 0;
}
