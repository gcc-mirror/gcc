struct s { int x[4]; };
struct s gs;

void
bar (void)
{
  struct s *s;
  int i;

  s = &gs;
  for (i = 0; i < 4; i++)
    ((char*) (&s->x[i]))[0] = 0;
}
