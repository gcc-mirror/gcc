/* { dg-do compile } */

typedef short unsigned int wchar_t;
typedef unsigned int size_t;
int* _errno(void);
int WideCharToMultiByte (wchar_t *);
int __attribute__ ((__nonnull__ (1)))
__wcrtomb_cp (char *dst, wchar_t wc, const unsigned int cp,
	      const unsigned int mb_max)
{
  if (cp == 0)     {
      if (wc > 255) 
	(*_errno()) = 42;
      return 1;
  }
  else 
    return WideCharToMultiByte (&wc);
}
void wcsrtombs (char *dst, const wchar_t *pwc, unsigned int cp,
		unsigned int mb_max)
{
  if ((__wcrtomb_cp (dst, *pwc, cp, mb_max)) <= 0)
    return;
  if ((__wcrtomb_cp (dst, *pwc, cp, mb_max)) <= 0)
    return;
}
