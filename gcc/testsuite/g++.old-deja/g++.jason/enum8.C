// { dg-do run  }
// Bug: the switch fails on the Alpha because folding ef - 1 fails.

enum foo { one=1, thirty=30 };

int f (enum foo ef)
{
  switch (ef)
    {
    case one:
    case thirty:
      return 0;
    default:
      return 1;
    }
}

int main ()
{
  return f (one);
}
