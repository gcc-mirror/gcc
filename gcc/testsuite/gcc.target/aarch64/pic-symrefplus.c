/* { dg-options "-O2 -mcmodel=small -fPIC -fno-builtin" }  */
/* { dg-do compile } */

typedef long unsigned int size_t;
enum
{
  __LC_TIME = 2,
};
enum
{
  ABDAY_1 = (((__LC_TIME) << 16) | (0)),
  DAY_1,
  ABMON_1,
  MON_1,
  D_T_FMT,
};
typedef struct __locale_struct
{
  struct locale_data *__locales[13];
} *__locale_t;
struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
};
struct locale_data
{
  const char *name;
  struct
  {
    const char *string;
  }
  values [];
};
extern const struct locale_data _nl_C_LC_TIME __attribute__ ((visibility ("hidden")));
char *
__strptime_internal (rp, fmt, tmp, statep , locale)
     const char *rp;
     const char *fmt;
     __locale_t locale;
     void *statep;
{
  struct locale_data *const current = locale->__locales[__LC_TIME];
  const char *rp_backup;
  const char *rp_longest;
  int cnt;
  size_t val;
  enum ptime_locale_status { not, loc, raw } decided_longest;
  struct __strptime_state
  {
    enum ptime_locale_status decided : 2;
  } s;
  struct tm tmb;
  struct tm *tm;
  if (statep == ((void *)0))
    {
      memset (&s, 0, sizeof (s));
    }
    {
      tm = &tmb;
    }
  while (*fmt != '\0')
    {
      if (*fmt != '%')
 {
   if (*fmt++ != *rp++) return ((void *)0);
   continue;
 }
      if (statep != ((void *)0))
 {
     ++fmt;
 }
      rp_backup = rp;
      switch (*fmt++)
 {
 case '%':
   for (cnt = 0; cnt < 7; ++cnt)
     {
       const char *trp;
       if (s.decided !=raw)
  {
    if (({ size_t len = strlen ((current->values[((int) (DAY_1 + cnt) & 0xffff)].string)); int result = __strncasecmp_l (((current->values[((int) (DAY_1 + cnt) & 0xffff)].string)), (trp), len, locale) == 0; if (result) (trp) += len; result; })
        && trp > rp_longest)
      {
      }
    if (({ size_t len = strlen ((current->values[((int) (ABDAY_1 + cnt) & 0xffff)].string)); int result = __strncasecmp_l (((current->values[((int) (ABDAY_1 + cnt) & 0xffff)].string)), (trp), len, locale) == 0; if (result) (trp) += len; result; })
        && trp > rp_longest)
      {
      }
  }
       if (s.decided != loc
    && (((trp = rp, ({ size_t len = strlen ((&_nl_C_LC_TIME.values[((int) (DAY_1) & 0xffff)].string)[cnt]); int result = __strncasecmp_l (((&_nl_C_LC_TIME.values[((int) (DAY_1) & 0xffff)].string)[cnt]), (trp), len, locale) == 0; if (result) (trp) += len; result; }))
         && trp > rp_longest)
        || ((trp = rp, ({ size_t len = strlen ((&_nl_C_LC_TIME.values[((int) (ABDAY_1) & 0xffff)].string)[cnt]); int result = __strncasecmp_l (((&_nl_C_LC_TIME.values[((int) (ABDAY_1) & 0xffff)].string)[cnt]), (rp), len, locale) == 0; if (result) (rp) += len; result; }))
     && trp > rp_longest)))
  {
  }
     }
     {
       const char *trp;
       if (s.decided != loc
    && (((trp = rp, ({ size_t len = strlen ((&_nl_C_LC_TIME.values[((int) (MON_1) & 0xffff)].string)[cnt]); int result = __strncasecmp_l (((&_nl_C_LC_TIME.values[((int) (MON_1) & 0xffff)].string)[cnt]), (trp), len, locale) == 0; if (result) (trp) += len; result; }))
         && trp > rp_longest)
        || ((trp = rp, ({ size_t len = strlen ((&_nl_C_LC_TIME.values[((int) (ABMON_1) & 0xffff)].string)[cnt]); int result = __strncasecmp_l (((&_nl_C_LC_TIME.values[((int) (ABMON_1) & 0xffff)].string)[cnt]), (trp), len, locale) == 0; if (result) (trp) += len; result; }))
     && trp > rp_longest)))
  {
  }
     }
 case 'c':
     {
       if (!(*((current->values[((int) (D_T_FMT) & 0xffff)].string)) != '\0' && (rp = __strptime_internal (rp, ((current->values[((int) (D_T_FMT) & 0xffff)].string)), tm, &s , locale)) != ((void *)0)))
  {
      rp = rp_backup;
  }
     }
 case 'C':
   do { int __n = 2; val = 0; while (*rp == ' ') ++rp; if (*rp < '0' || *rp > '9') return ((void *)0); do { val *= 10; val += *rp++ - '0'; } while (--__n > 0 && val * 10 <= 99 && *rp >= '0' && *rp <= '9'); if (val < 0 || val > 99) return ((void *)0); } while (0);
 case 'F':
   if (!(*("%Y-%m-%d") != '\0' && (rp = __strptime_internal (rp, ("%Y-%m-%d"), tm, &s , locale)) != ((void *)0)))
   tm->tm_hour = val % 12;
 }
    }
}
char *
__strptime_l (buf, format, tm , locale)
{
}
