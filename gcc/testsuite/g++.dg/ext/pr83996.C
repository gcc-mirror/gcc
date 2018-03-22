// PR c++/83996
// { dg-do compile }
// { dg-options "" }

int z[] = { };

int
main (void)
{
  __builtin_printf ("%d\n", *(z + 1));
}
