/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

typedef long unsigned int size_t;
typedef int wchar_t;
struct tm
{
  int tm_mon;
  int tm_year;
};
int abs (int);
struct lc_time_T { const char *month[12]; };
struct __locale_t * __get_current_locale (void) { }
const struct lc_time_T * __get_time_locale (struct __locale_t *locale) { }
const wchar_t * __ctloc (wchar_t *buf, const char *elem, size_t *len_ret) { return buf; }
size_t
__strftime (wchar_t *s, size_t maxsize, const wchar_t *format,
     const struct tm *tim_p, struct __locale_t *locale)
{
  size_t count = 0;
  const wchar_t *ctloc;
  wchar_t ctlocbuf[256];
  size_t i, ctloclen;
  const struct lc_time_T *_CurrentTimeLocale = __get_time_locale (locale);
    {
      switch (*format)
 {
 case L'B':
   (ctloc = __ctloc (ctlocbuf, _CurrentTimeLocale->month[tim_p->tm_mon], &ctloclen));
   for (i = 0; i < ctloclen; i++)
     {
       if (count < maxsize - 1)
  s[count++] = ctloc[i];
       else
  return 0;
       {
  int century = tim_p->tm_year >= 0
    ? tim_p->tm_year / 100 + 1900 / 100
    : abs (tim_p->tm_year + 1900) / 100;
       }
   }
 }
    }
}
