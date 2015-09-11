/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

extern char *bar[17];

int foo(int argc, char **argv)
{
  int i;
  int n = 0;

  for (i = 0; i < argc; i++)
    n++;

  for (i = 0; i < argc; i++)
    argv[i] = bar[i + n]; /* { dg-bogus "above array bounds" } */

  return 0;
}
