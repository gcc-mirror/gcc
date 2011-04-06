/* PR middle-end/48335 */

struct S { float d; };

void bar (struct S);

void
f0 (int x)
{
  struct S s = {.d = 0.0f };
  ((char *) &s.d)[0] = x;
  s.d *= 7.0;
  bar (s);
}

void
f1 (int x)
{
  struct S s = {.d = 0.0f };
  ((char *) &s.d)[1] = x;
  s.d *= 7.0;
  bar (s);
}

void
f2 (int x)
{
  struct S s = {.d = 0.0f };
  ((char *) &s.d)[2] = x;
  s.d *= 7.0;
  bar (s);
}

void
f3 (int x)
{
  struct S s = {.d = 0.0f };
  ((char *) &s.d)[3] = x;
  s.d *= 7.0;
  bar (s);
}
