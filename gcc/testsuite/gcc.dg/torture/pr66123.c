/* { dg-do compile } */

int
test (int foo)
{
  static void *dummy[] = { &&a, &&b };
  goto *((char *) &&b - 2 * (foo < 0));
a:
b:
  return 0;
}
