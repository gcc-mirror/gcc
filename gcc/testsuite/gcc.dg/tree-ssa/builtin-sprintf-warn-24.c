/* PR middle-end/81401 - false positive -Wformat-overflow in a loop
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow" } */

char a[3];

void f (void)
{
  int i, i0 = 0x00;

  for (i = i0; i <= 0xff; ++i)
    __builtin_sprintf (a, "%02x", i);   // { dg-bogus "\\\[-Wformat-overflow" }
}

char b[2];

void g (void)
{
  int i;
  for (i = 0; i < 10; ++i)
    __builtin_sprintf (b, "%d", i);     // { dg-bogus "\\\[-Wformat-overflow" }
}
