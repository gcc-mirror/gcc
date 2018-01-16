int yf = 0;

void
pl (int q5, int nd)
{
  unsigned int hp = q5;
  int zx = (q5 == 0) ? hp : (hp / q5);

  yf = ((nd < 2) * zx != 0) ? nd : 0;
}

int
main (void)
{
  pl (1, !yf);
  if (yf != 1)
    __builtin_abort ();

  return 0;
}


