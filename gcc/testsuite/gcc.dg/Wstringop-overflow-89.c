/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline -Wall" } */

extern void process (char);

void process_array4 (char a[4], int n)
{
  for (int i = 0; i < n; i++)
    process (a[i]);
}

void process_array3 (char a[3], int n)
{
  for (int i = 0; i < n; i++)
    process (a[i]);
}
