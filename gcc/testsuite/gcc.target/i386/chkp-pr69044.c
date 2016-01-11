/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */

int i;
int strncasecmp (char *p1, char *p2, long p3) { return 0; }
int special_command ()
{
  if (strncasecmp (0, 0, 0))
    i++;
}
