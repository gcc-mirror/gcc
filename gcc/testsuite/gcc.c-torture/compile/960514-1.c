struct s {
  unsigned long long t[5];
};

void
f (struct s *d, unsigned long long *l)
{
  int i;

  for (i = 0; i < 5; i++)
    d->t[i] += l[i];
}
