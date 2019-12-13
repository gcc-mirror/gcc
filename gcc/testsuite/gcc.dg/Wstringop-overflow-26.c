/* PR middle-end/79221 - missing -Wstringop-overflow= on a strcat overflow
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern char* strcat (char*, const char*);

char d[3];

void f (int i)
{
  const char *s = i < 0 ? "01234567" : "89abcd";
  strcat (d, s);    // { dg-warning "'strcat' writing between 7 and 9 bytes into a region of size 3" } */
}


void g (int i)
{
  const char *s = i < 0 ? "12345678" : "87654321";
  strcat (d, s);    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
}
