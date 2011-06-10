/* { dg-do compile } */
/* { dg-options "-O2 -fdump-passes" } */

unsigned res;

void
foo (unsigned code, int len)
{
  int i;
  for (i = 0; i < len; i++)
    res |= code & 1;
}

/* { dg-prune-output ".*" } */
