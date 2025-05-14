/* PR middle-end/119226 */

char a[64];
void bar (void);

void
foo (int x)
{
  char *b = a + __builtin_strcspn (a, x ? "" : "ab");
  if (b[0])
    bar ();
}
