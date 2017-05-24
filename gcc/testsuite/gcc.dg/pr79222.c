/* PR middle-end/79222 - missing -Wstringop-overflow= on a stpcpy overflow
   { dg-do compile }
   { dg-options "-O2" } */

extern char* stpcpy (char*, const char*);

char d[3];

char* f (int i)
{
  const char *s = i < 0 ? "01234567" : "9876543210";
  return stpcpy (d, s);   /* { dg-warning ".stpcpy. writing between 9 and 11 bytes into a region of size 3 overflows the destination" } */
}
