/* PR middle-end/38343 */

static struct S
{
  char f[6];
} s[] = { {"01000"} };

char *
foo (void)
{
  return __builtin_stpcpy (s[0].f, "S0022");
}
