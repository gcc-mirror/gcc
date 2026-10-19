/* { dg-do compile } */
/* { dg-options "-fexceptions -fstack-check -mbackchain" } */

void a (int *);
int b (char *);
void c (void)
{
  for (;;)
    {
      char d[88];
      int e __attribute__ ((cleanup (a))) = 0;
      b (d);
    }
}
