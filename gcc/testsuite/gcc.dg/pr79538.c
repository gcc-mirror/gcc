/* PR middle-end/79538 - missing -Wformat-overflow with %s and non-member array arguments
   { dg-do compile }
   { dg-options "-O2 -Wformat-overflow" } */

char a3[3];
char a4[4];
char d[3];

void g (int i)
{
  const char *s = i < 0 ? a3 : a4;
  __builtin_sprintf (d, "%s", s);      /* { dg-warning ".__builtin_sprintf. may write a terminating nul past the end of the destination" } */
  return;
}

void f ()
{
  char des[3];
  char src[] = "abcd";
  __builtin_sprintf (des, "%s", src); /* { dg-warning "directive writing up to 4 bytes into a region of size 3" } */
  return;
}
