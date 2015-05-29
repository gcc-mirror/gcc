/* PR tree-optimization/45633 */
/* { dg-do compile } */

int s[32];
unsigned char *t[32];

void
foo (void)
{
  int i;
  for (i = 0; i < 32; i++)
    t[i] -= s[i];
}

