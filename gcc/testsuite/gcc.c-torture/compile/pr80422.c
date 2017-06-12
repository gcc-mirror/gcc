
int a, c, f;
short b, d, e;

int fn1 (int h)
{ 
  return a > 2 || h > a ? h : h << a;
}

void fn2 ()
{ 
  int j, k;
  while (1)
    { 
      k = c && b;
      f &= e > (fn1 (k) && j);
      if (!d)
        break;
    }
}

int main ()
{ 
  fn2 ();
  return 0;
}
