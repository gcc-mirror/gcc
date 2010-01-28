struct C
{
  ~C ();
  int c3;
};

C *b2;

static void
b1 (const C &x, unsigned b3, unsigned b4)
{
  unsigned i = 0;
  for (; i < b3; i++)
    if (i < b4)
      {
        b2[0].c3 = x.c3;
        return;
      }
}

int a ();

void
bar (unsigned b3, unsigned b4)
{
  C c[100];
  for (int i = 0; i < 100; i++)
    {
      c[i].c3 = i;
      for (int j = 0; j < b3; j++)
        if (j < b4)
          {
            b2[0].c3 = 0;
            break;
          }
      b1 (c[i], b3, b4);
      a ();
    }
}

