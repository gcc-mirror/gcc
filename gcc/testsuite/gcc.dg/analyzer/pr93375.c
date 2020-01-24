/* { dg-additional-options "-Wno-implicit-int" } */

void
en (jm)
{
}

void
p2 ()
{
  char *rl = 0;

  en ();
  __builtin_memcpy (rl, 0, sizeof (0)); /* { dg-warning "dereference of NULL" } */
}
