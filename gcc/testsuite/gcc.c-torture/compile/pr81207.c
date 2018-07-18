/* PR middle-end/81207 */

static const char *b[2] = { "'", "" };

int
foo (const char *d)
{
  int e;
  for (e = 0; b[e]; e++)
    if (__builtin_strstr (d, b[e]))
      return 1;
  return 0;
}
