/* PR middle-end/78138 - missing warnings on buffer overflow with non-constant
   source length
   { dg-do compile }
   { dg-options "-O2 -Wformat-overflow" } */

char d [5];

void* memcpy (void*, const void*, unsigned long);
extern char* strcpy (char*, const char*);

void f (int i, int j)
{
  strcpy (d, j ? "12345" : "123456");   /* { dg-warning ".strcpy. writing 6 bytes into a region of size 5" } */
}

void g (void *p)
{
  extern unsigned n;
  if (n < 17 || 32 < n) n = 7;

  memcpy (d, p, n);   /* { dg-warning ".memcpy.: writing between 7 and 32 bytes into a region of size 5" } */
};
