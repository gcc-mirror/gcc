/* PR middle-end/89957 - ICE calling strnlen with an int128_t bound
   in a known range
   PR middle-end/89911 - ICE on a call with no arguments to strnlen
   declared with no prototype
   { dg-do compile }
   { dg-options "-O2 -fpermissive -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern size_t strnlen ();

size_t f0 (void)
{
  return strnlen ();          /* { dg-warning "too few arguments to built-in function 'strnlen'" } */
}

size_t f1 (const char *s)
{
  return strnlen (s);         /* { dg-warning "too few arguments to built-in function 'strnlen'" } */
}

size_t f2 (const char *s)
{
  return strnlen (s, s);      /* { dg-warning "\\\[-Wint-conversion]" } */
}

#if __SIZEOF_INT128__ == 16

size_t fi128 (const char *s, __int128_t n)
{
 if (n < 0)
   n = 0;

 /* PR middle-end/89957 */
 return strnlen (s, n);       /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" "int128" { target int128 } } */
}

#endif

/* { dg-prune-output "\\\[-Wint-conversion]" } */
