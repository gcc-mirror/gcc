/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */
/* { dg-additional-options "-fpermissive" } */

int foo;
typedef long unsigned int size_t;
typedef short unsigned int wchar_t;
extern int swprintf (wchar_t *wcs, size_t maxlen, const wchar_t *format, ...);
struct tm
{
  int tm_mday;
  int tm_mon;
  int tm_year;
};
size_t
__strftime (wchar_t * s, size_t maxsize, const wchar_t * format, const struct tm *tim_p)
{
  size_t count = 0;
  int len = 0;
  size_t i, ctloclen;
  unsigned long width;
  {
    if (foo)
      {
	{
	  wchar_t *fmt = L"%s%.*d";
	  len = swprintf (&s[count], maxsize, fmt, "-", width, 0);
	}
	if ((count) >= maxsize)
	  return 0;
      }
    else
      {
	len =
	  swprintf (&s[count], maxsize - count, L"%.2d/%.2d/%.2d", 42, 99, 0);
	if ((count) >= maxsize)
	  return 0;

      }
  }
}
