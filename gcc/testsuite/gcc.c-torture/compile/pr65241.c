enum E { A, B, C, D };
void fn4 (void);

int
fn1 (enum E p1)
{
  static int w[D];
  if (w[p1])
    switch (p1)
      case C:
      w[p1] = 0;
}

void
fn2 (p1)
{
  fn1 (p1);
}

void
fn3 (enum E p1)
{
  fn2 (p1);
  fn4 ();
  fn2 (p1);
}
