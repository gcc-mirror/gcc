
int a[1], b, c, d, e, f, g, h;

void fn1 (int p)
{ 
  b = b >> 8 ^ a[b ^ (c & 5)] >> 8 ^ a[(b ^ c) & 5];
  b = b >> 8 ^ a[(b ^ c) & 5];
}

static void fn2 ()
{ 
  int k;
  while (1)
    while (e)
      { 
        while (g)
          while (h)
            for (k = 0; k < 6; k++)
              while (f)
                fn1 (0);
        fn1 (0);
        fn1 (0);
        fn1 (0);
      }
}

int main ()
{ 
  fn2 ();
  return 0;
}
