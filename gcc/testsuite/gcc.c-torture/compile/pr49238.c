/* PR target/49238 */
extern int bar (void);

void
foo (unsigned long long a, int b)
{
  int i;

  if (b)
    for (a = -12; a >= 10; a = bar ())
      break;
  else
    return;

  for (i = 0; i < 10; i += 10)
    if ((i == bar ()) | (bar () >= a))
      bar ();
}
