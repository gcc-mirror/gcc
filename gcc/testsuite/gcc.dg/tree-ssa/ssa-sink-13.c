/* PR33315 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink" } */

int num;
int a[20];

void test ()
{
  int i;
  int *ptr;
  ptr = & a[0];
  i = num;
  if ( i == 1) *(ptr+0) = 0;
  if ( i != 1) *(ptr+0) = 0;
  if ( i == 2) *(ptr+1) = 0;
  if ( i != 2) *(ptr+1) = 0;
  if ( i == 3) *(ptr+2) = 0;
  if ( i != 3) *(ptr+2) = 0;
}

/* We should sink/merge all stores and end up with a single BB.  */

/* { dg-final { scan-tree-dump-times "MEM\[^\n\r\]* = 0;" 3 "sink" } } */
/* { dg-final { scan-tree-dump-times "<bb " 1 "sink" } } */
