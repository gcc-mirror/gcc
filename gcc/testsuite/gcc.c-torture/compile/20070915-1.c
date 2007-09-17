/* PR middle-end/33423 */

static struct
{
  char buf[15];
} u2;

void
test6 (void)
{
  int len;
  char *p;

  for (len = 0; len < 2; len++)
    {
      p = __builtin___memset_chk (u2.buf, '\0', len, 15);
      if (p != u2.buf)
	return;
    }
}
