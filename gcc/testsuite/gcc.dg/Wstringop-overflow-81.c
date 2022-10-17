/* Verify that -Wstringop-overflow uses context-sensitive range info
   even at -O0.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

extern void* memset (void*, int, __SIZE_TYPE__);

char a[8];

void warn_offset_range (int i)
{
  if (i < 4)
    i = 4;
  memset (a + i, 0, 5);       // { dg-warning "writing 5 bytes into a region of size 4 " }
}

void warn_size_range (int i, int n)
{
  if (n < 5)
    n = 5;

  memset (a + 4, 1, n);      // { dg-warning "writing between 5 and \\d+ bytes into a region of size 4 " }
}

void warn_offset_and_size_range (int i, int n)
{
  if (n < 5)
    n = 5;

  if (i < 4)
    {
      if (n < 9)
	n = 9;
      memset (a + i, 1, n);   // { dg-warning "writing between 9 and \\d+ bytes into a region of size 8 " }
    }
  else
    memset (a + i, 0, n);     // { dg-warning "writing between 5 and \\d+ bytes into a region of size 4 " }
}
