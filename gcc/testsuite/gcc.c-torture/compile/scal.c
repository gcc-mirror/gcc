/* { dg-require-effective-target untyped_assembly } */
/* { dg-additional-options "-std=gnu89" } */

int g1, g2;

void
write_at (addr, off, val)
     int *addr;
     int off;
     int val;
{
  g2 = 1;
  addr[off] = val;
  g2++;
}

main ()
{
  g2 = 12;
  write_at (&g1, &g2 - &g1, 12345);
  printf ("%d\n", g2);
}
