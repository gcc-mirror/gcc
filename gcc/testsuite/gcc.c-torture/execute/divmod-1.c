div1 (x)
     signed char x;
{
  return x / -1;
}

div2 (x)
     signed short x;
{
  return x / -1;
}

div3 (x, y)
     signed char x;
     signed char y;
{
  return x / y;
}

div4 (x, y)
     signed short x;
     signed short y;
{
  return x / y;
}

mod1 (x)
     signed char x;
{
  return x % -1;
}

mod2 (x)
     signed short x;
{
  return x % -1;
}

mod3 (x, y)
     signed char x;
     signed char y;
{
  return x % y;
}

mod4 (x, y)
     signed short x;
     signed short y;
{
  return x % y;
}

signed long
mod5 (x, y)
     signed long x;
     signed long y;
{
  return x % y;
}
     
unsigned long
mod6 (x, y)
     unsigned long x;
     unsigned long y;
{
  return x % y;
}
     
main ()
{
  if (div1 (-(1 << 7)) != 1 << 7)
    abort ();
  if (div2 (-(1 << 15)) != 1 << 15)
    abort ();
  if (div3 (-(1 << 7), -1) != 1 << 7)
    abort ();
  if (div4 (-(1 << 15), -1) != 1 << 15)
    abort ();
  if (mod1 (-(1 << 7)) != 0)
    abort ();
  if (mod2 (-(1 << 15)) != 0)
    abort ();
  if (mod3 (-(1 << 7), -1) != 0)
    abort ();
  if (mod4 (-(1 << 15), -1) != 0)
    abort ();
  if (mod5 (0x50000000, 2) != 0)
    abort ();
  if (mod6 (0x50000000, 2) != 0)
    abort ();
  
  exit (0);
}
