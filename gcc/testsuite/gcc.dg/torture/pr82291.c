/* { dg-do run } */

int a, c, d, *h;
unsigned b;

int *fn1 ()
{ 
  int *f[3], g = 0;
  for (; g < 3; g++)
    f[g] = &a;
  if (--b > a)
    { 
      if (a > b)
	d++;
      return f[0];
    }
}

void fn2 ()
{ 
  for (; c >= 0; --c)
    { 
      int j[] = { 0, 0, 0, 0, 0 };
      int *k = fn1 ();
      if (!k)
	__builtin_abort ();
      h = &j[4];
    }
}

int main ()
{ 
  fn2 ();
  if (d != 0)
    __builtin_abort ();
  return 0;
}
