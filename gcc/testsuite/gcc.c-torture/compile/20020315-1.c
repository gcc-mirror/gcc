/* PR bootstrap/4128 */

extern int bar (char *, char *, int, int);
extern long baz (char *, char *, int, int);

int sgt (char *a, char *b, int c, int d)
{
  return bar (a, b, c, d) > 0;
}

long dgt (char *a, char *b, int c, int d)
{
  return baz (a, b, c, d) > 0;
}

int sne (char *a, char *b, int c, int d)
{
  return bar (a, b, c, d) != 0;
}

long dne (char *a, char *b, int c, int d)
{
  return baz (a, b, c, d) != 0;
}

int seq (char *a, char *b, int c, int d)
{
  return bar (a, b, c, d) == 0;
}

long deq (char *a, char *b, int c, int d)
{
  return baz (a, b, c, d) == 0;
}
