// PR sanitizer/80386
// { dg-do run }
// { dg-options "-fsanitize=undefined -fno-sanitize-recover" }

static unsigned long long int i = 13996271126042720493ULL;

int
main ()
{
  int r = (((2921 + 0) - short(i)) + 0x7fffffff) >> 0;
  asm volatile ("" : "+g" (r));
  return 0;
}
