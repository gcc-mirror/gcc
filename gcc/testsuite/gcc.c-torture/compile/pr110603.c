/* PR tree-optimization/110603 */

typedef __SIZE_TYPE__ size_t;
void *memcpy (void *, const void *, size_t);
int snprintf (char *restrict, size_t, const char *restrict, ...);
extern char a[2];
void bar (void);

void
foo (void)
{
  memcpy (a, "12", sizeof (a));
  int b = snprintf (0, 0, "%s", a);
  if (b <= 3)
    bar ();
}
