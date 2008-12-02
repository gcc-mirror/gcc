/* PR middle-end/38343 */

static struct A
{
  char f[6];
} a[] = { {"01000"} };

void
foo (void)
{
  __builtin_stpcpy (a[0].f, "S0022");
}
