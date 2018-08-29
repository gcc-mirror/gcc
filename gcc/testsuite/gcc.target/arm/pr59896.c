/* { dg-do compile } */
/* { dg-skip-if "incompatible options" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-mthumb -O2" } */

typedef unsigned int size_t;
typedef unsigned int wchar_t;
typedef int ptrdiff_t;
typedef signed char __int8_t ;
typedef unsigned char __uint8_t ;
typedef signed short __int16_t;
typedef unsigned short __uint16_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef signed long long __int64_t;
typedef unsigned long long __uint64_t;
typedef int _LOCK_T;
typedef int _LOCK_RECURSIVE_T;
typedef long _off_t;
typedef short __dev_t;
typedef unsigned short __uid_t;
typedef unsigned short __gid_t;
__extension__ typedef long long _off64_t;
typedef long _fpos_t;
typedef signed int _ssize_t;
typedef unsigned int wint_t;
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    unsigned char __wchb[4];
  } __value;
} _mbstate_t;
typedef _LOCK_RECURSIVE_T _flock_t;
typedef void *_iconv_t;
typedef unsigned long __ULong;
struct _reent;
struct _Bigint
{
  struct _Bigint *_next;
  int _k, _maxwds, _sign, _wds;
  __ULong _x[1];
};
struct __tm
{
  int __tm_sec;
  int __tm_min;
  int __tm_hour;
  int __tm_mday;
  int __tm_mon;
  int __tm_year;
  int __tm_wday;
  int __tm_yday;
  int __tm_isdst;
};
struct _on_exit_args {
 void * _fnargs[32];
 void * _dso_handle[32];
 __ULong _fntypes;
 __ULong _is_cxa;
};
struct _atexit {
 struct _atexit *_next;
 int _ind;
 void (*_fns[32])(void);
        struct _on_exit_args _on_exit_args;
};
struct __sbuf {
 unsigned char *_base;
 int _size;
};
struct __sFILE {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;
  void * _cookie;
  int (* _read) (struct _reent *, void *, char *, int)
                                          ;
  int (* _write) (struct _reent *, void *, const char *, int)
                                   ;
  _fpos_t (* _seek) (struct _reent *, void *, _fpos_t, int);
  int (* _close) (struct _reent *, void *);
  struct __sbuf _ub;
  unsigned char *_up;
  int _ur;
  unsigned char _ubuf[3];
  unsigned char _nbuf[1];
  struct __sbuf _lb;
  int _blksize;
  _off_t _offset;
  struct _reent *_data;
  _flock_t _lock;
  _mbstate_t _mbstate;
  int _flags2;
};
typedef struct __sFILE __FILE;
struct _glue
{
  struct _glue *_next;
  int _niobs;
  __FILE *_iobs;
};
struct _rand48 {
  unsigned short _seed[3];
  unsigned short _mult[3];
  unsigned short _add;
};
struct _reent
{
  int _errno;
  __FILE *_stdin, *_stdout, *_stderr;
  int _inc;
  char _emergency[25];
  int _current_category;
  const char *_current_locale;
  int __sdidinit;
  void (* __cleanup) (struct _reent *);
  struct _Bigint *_result;
  int _result_k;
  struct _Bigint *_p5s;
  struct _Bigint **_freelist;
  int _cvtlen;
  char *_cvtbuf;
  union
    {
      struct
        {
          unsigned int _unused_rand;
          char * _strtok_last;
          char _asctime_buf[26];
          struct __tm _localtime_buf;
          int _gamma_signgam;
          __extension__ unsigned long long _rand_next;
          struct _rand48 _r48;
          _mbstate_t _mblen_state;
          _mbstate_t _mbtowc_state;
          _mbstate_t _wctomb_state;
          char _l64a_buf[8];
          char _signal_buf[24];
          int _getdate_err;
          _mbstate_t _mbrlen_state;
          _mbstate_t _mbrtowc_state;
          _mbstate_t _mbsrtowcs_state;
          _mbstate_t _wcrtomb_state;
          _mbstate_t _wcsrtombs_state;
   int _h_errno;
        } _reent;
      struct
        {
          unsigned char * _nextf[30];
          unsigned int _nmalloc[30];
        } _unused;
    } _new;
  struct _atexit *_atexit;
  struct _atexit _atexit0;
  void (**(_sig_func))(int);
  struct _glue __sglue;
  __FILE __sf[3];
};
extern struct _reent *_impure_ptr ;
extern struct _reent *const _global_impure_ptr ;
void _reclaim_reent (struct _reent *);
typedef struct
{
  int quot;
  int rem;
} div_t;
typedef struct
{
  long quot;
  long rem;
} ldiv_t;
typedef struct
{
  long long int quot;
  long long int rem;
} lldiv_t;
typedef int (*__compar_fn_t) (const void *, const void *);
int __locale_mb_cur_max (void);
void abort (void) __attribute__ ((noreturn));
int abs (int);
int atexit (void (*__func)(void));
double atof (const char *__nptr);
float atoff (const char *__nptr);
int atoi (const char *__nptr);
int _atoi_r (struct _reent *, const char *__nptr);
long atol (const char *__nptr);
long _atol_r (struct _reent *, const char *__nptr);
void * bsearch (const void * __key, const void * __base, size_t __nmemb, size_t __size, __compar_fn_t _compar)
                                ;
void * calloc (size_t __nmemb, size_t __size) ;
div_t div (int __numer, int __denom);
void exit (int __status) __attribute__ ((noreturn));
void free (void *) ;
char * getenv (const char *__string);
char * _getenv_r (struct _reent *, const char *__string);
char * _findenv (const char *, int *);
char * _findenv_r (struct _reent *, const char *, int *);
extern char *suboptarg;
int getsubopt (char **, char * const *, char **);
long labs (long);
ldiv_t ldiv (long __numer, long __denom);
void * malloc (size_t __size) ;
int mblen (const char *, size_t);
int _mblen_r (struct _reent *, const char *, size_t, _mbstate_t *);
int mbtowc (wchar_t *, const char *, size_t);
int _mbtowc_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
int wctomb (char *, wchar_t);
int _wctomb_r (struct _reent *, char *, wchar_t, _mbstate_t *);
size_t mbstowcs (wchar_t *, const char *, size_t);
size_t _mbstowcs_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
size_t wcstombs (char *, const wchar_t *, size_t);
size_t _wcstombs_r (struct _reent *, char *, const wchar_t *, size_t, _mbstate_t *);
char * mkdtemp (char *);
int mkostemp (char *, int);
int mkostemps (char *, int, int);
int mkstemp (char *);
int mkstemps (char *, int);
char * mktemp (char *) __attribute__ ((__warning__ ("the use of `mktemp' is dangerous; use `mkstemp' instead")));
char * _mkdtemp_r (struct _reent *, char *);
int _mkostemp_r (struct _reent *, char *, int);
int _mkostemps_r (struct _reent *, char *, int, int);
int _mkstemp_r (struct _reent *, char *);
int _mkstemps_r (struct _reent *, char *, int);
char * _mktemp_r (struct _reent *, char *) __attribute__ ((__warning__ ("the use of `mktemp' is dangerous; use `mkstemp' instead")));
void qsort (void * __base, size_t __nmemb, size_t __size, __compar_fn_t _compar);
int rand (void);
void * realloc (void * __r, size_t __size) ;
void * reallocf (void * __r, size_t __size);
void srand (unsigned __seed);
double strtod (const char *__n, char **__end_PTR);
double _strtod_r (struct _reent *,const char *__n, char **__end_PTR);
float strtof (const char *__n, char **__end_PTR);
long strtol (const char *__n, char **__end_PTR, int __base);
long _strtol_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
unsigned long strtoul (const char *__n, char **__end_PTR, int __base);
unsigned long _strtoul_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
int system (const char *__string);
long a64l (const char *__input);
char * l64a (long __input);
char * _l64a_r (struct _reent *,long __input);
int on_exit (void (*__func)(int, void *),void * __arg);
void _Exit (int __status) __attribute__ ((noreturn));
int putenv (char *__string);
int _putenv_r (struct _reent *, char *__string);
void * _reallocf_r (struct _reent *, void *, size_t);
int setenv (const char *__string, const char *__value, int __overwrite);
int _setenv_r (struct _reent *, const char *__string, const char *__value, int __overwrite);
char * gcvt (double,int,char *);
char * gcvtf (float,int,char *);
char * fcvt (double,int,int *,int *);
char * fcvtf (float,int,int *,int *);
char * ecvt (double,int,int *,int *);
char * ecvtbuf (double, int, int*, int*, char *);
char * fcvtbuf (double, int, int*, int*, char *);
char * ecvtf (float,int,int *,int *);
char * dtoa (double, int, int, int *, int*, char**);
int rand_r (unsigned *__seed);
double drand48 (void);
double _drand48_r (struct _reent *);
double erand48 (unsigned short [3]);
double _erand48_r (struct _reent *, unsigned short [3]);
long jrand48 (unsigned short [3]);
long _jrand48_r (struct _reent *, unsigned short [3]);
void lcong48 (unsigned short [7]);
void _lcong48_r (struct _reent *, unsigned short [7]);
long lrand48 (void);
long _lrand48_r (struct _reent *);
long mrand48 (void);
long _mrand48_r (struct _reent *);
long nrand48 (unsigned short [3]);
long _nrand48_r (struct _reent *, unsigned short [3]);
unsigned short *
       seed48 (unsigned short [3]);
unsigned short *
       _seed48_r (struct _reent *, unsigned short [3]);
void srand48 (long);
void _srand48_r (struct _reent *, long);
long long atoll (const char *__nptr);
long long _atoll_r (struct _reent *, const char *__nptr);
long long llabs (long long);
lldiv_t lldiv (long long __numer, long long __denom);
long long strtoll (const char *__n, char **__end_PTR, int __base);
long long _strtoll_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
unsigned long long strtoull (const char *__n, char **__end_PTR, int __base);
unsigned long long _strtoull_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
void cfree (void *);
int unsetenv (const char *__string);
int _unsetenv_r (struct _reent *, const char *__string);
char * _dtoa_r (struct _reent *, double, int, int, int *, int*, char**);
void * _malloc_r (struct _reent *, size_t) ;
void * _calloc_r (struct _reent *, size_t, size_t) ;
void _free_r (struct _reent *, void *) ;
void * _realloc_r (struct _reent *, void *, size_t) ;
void _mstats_r (struct _reent *, char *);
int _system_r (struct _reent *, const char *);
void __eprintf (const char *, const char *, unsigned int, const char *);
extern long double strtold (const char *, char **);
extern long double wcstold (const wchar_t *, wchar_t **);
typedef long int __off_t;
typedef int __pid_t;
__extension__ typedef long long int __loff_t;
struct stat;
struct tms;
struct timeval;
struct timezone;
extern int _close_r (struct _reent *, int);
extern int _execve_r (struct _reent *, const char *, char *const *, char *const *);
extern int _fcntl_r (struct _reent *, int, int, int);
extern int _fork_r (struct _reent *);
extern int _fstat_r (struct _reent *, int, struct stat *);
extern int _getpid_r (struct _reent *);
extern int _isatty_r (struct _reent *, int);
extern int _kill_r (struct _reent *, int, int);
extern int _link_r (struct _reent *, const char *, const char *);
extern _off_t _lseek_r (struct _reent *, int, _off_t, int);
extern int _mkdir_r (struct _reent *, const char *, int);
extern int _open_r (struct _reent *, const char *, int, int);
extern _ssize_t _read_r (struct _reent *, int, void *, size_t);
extern int _rename_r (struct _reent *, const char *, const char *);
extern void *_sbrk_r (struct _reent *, ptrdiff_t);
extern int _stat_r (struct _reent *, const char *, struct stat *);
extern unsigned long _times_r (struct _reent *, struct tms *);
extern int _unlink_r (struct _reent *, const char *);
extern int _wait_r (struct _reent *, int *);
extern _ssize_t _write_r (struct _reent *, int, const void *, size_t);
extern int _gettimeofday_r (struct _reent *, struct timeval *__tp, void *__tzp);
typedef signed char int8_t ;
typedef unsigned char uint8_t ;
typedef signed char int_least8_t;
typedef unsigned char uint_least8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef int16_t int_least16_t;
typedef uint16_t uint_least16_t;
typedef signed long int32_t;
typedef unsigned long uint32_t;
typedef int32_t int_least32_t;
typedef uint32_t uint_least32_t;
typedef signed long long int64_t;
typedef unsigned long long uint64_t;
typedef int64_t int_least64_t;
typedef uint64_t uint_least64_t;
  typedef signed int int_fast8_t;
  typedef unsigned int uint_fast8_t;
  typedef signed int int_fast16_t;
  typedef unsigned int uint_fast16_t;
  typedef signed int int_fast32_t;
  typedef unsigned int uint_fast32_t;
  typedef int_least64_t int_fast64_t;
  typedef uint_least64_t uint_fast64_t;
  typedef long long int intmax_t;
  typedef long long unsigned int uintmax_t;
typedef signed int intptr_t;
typedef unsigned int uintptr_t;
void * memchr (const void *, int, size_t);
int memcmp (const void *, const void *, size_t);
void * memcpy (void * , const void * , size_t);
void * memmove (void *, const void *, size_t);
void * memset (void *, int, size_t);
char *strcat (char *, const char *);
char *strchr (const char *, int);
int strcmp (const char *, const char *);
int strcoll (const char *, const char *);
char *strcpy (char *, const char *);
size_t strcspn (const char *, const char *);
char *strerror (int);
size_t strlen (const char *);
char *strncat (char *, const char *, size_t);
int strncmp (const char *, const char *, size_t);
char *strncpy (char *, const char *, size_t);
char *strpbrk (const char *, const char *);
char *strrchr (const char *, int);
size_t strspn (const char *, const char *);
char *strstr (const char *, const char *);
char *strtok (char *, const char *);
size_t strxfrm (char *, const char *, size_t);
char *strtok_r (char *, const char *, char **);
int bcmp (const void *, const void *, size_t);
void bcopy (const void *, void *, size_t);
void bzero (void *, size_t);
int ffs (int);
char *index (const char *, int);
void * memccpy (void * , const void * , int, size_t);
void * mempcpy (void *, const void *, size_t);
void * memmem (const void *, size_t, const void *, size_t);
void * memrchr (const void *, int, size_t);
void * rawmemchr (const void *, int);
char *rindex (const char *, int);
char *stpcpy (char *, const char *);
char *stpncpy (char *, const char *, size_t);
int strcasecmp (const char *, const char *);
char *strcasestr (const char *, const char *);
char *strchrnul (const char *, int);
char *strdup (const char *);
char *_strdup_r (struct _reent *, const char *);
char *strndup (const char *, size_t);
char *_strndup_r (struct _reent *, const char *, size_t);
int strerror_r (int, char *, size_t) __asm__ ("" "__xpg_strerror_r");
size_t strlcat (char *, const char *, size_t);
size_t strlcpy (char *, const char *, size_t);
int strncasecmp (const char *, const char *, size_t);
size_t strnlen (const char *, size_t);
char *strsep (char **, const char *);
char *strlwr (char *);
char *strupr (char *);
char *strsignal (int __signo);
char * _strerror_r (struct _reent *, int, int, int *);
typedef union
{
  double value;
  struct
  {
    unsigned int fraction1:32;
    unsigned int fraction0:20;
    unsigned int exponent :11;
    unsigned int sign : 1;
  } number;
  struct
  {
    unsigned int function1:32;
    unsigned int function0:19;
    unsigned int quiet:1;
    unsigned int exponent: 11;
    unsigned int sign : 1;
  } nan;
  struct
  {
    unsigned long lsw;
    unsigned long msw;
  } parts;
  long aslong[2];
} __ieee_double_shape_type;
typedef union
{
  float value;
  struct
  {
    unsigned int fraction0: 7;
    unsigned int fraction1: 16;
    unsigned int exponent: 8;
    unsigned int sign : 1;
  } number;
  struct
  {
    unsigned int function1:16;
    unsigned int function0:6;
    unsigned int quiet:1;
    unsigned int exponent:8;
    unsigned int sign:1;
  } nan;
  long p1;
} __ieee_float_shape_type;
typedef int fp_rnd;
fp_rnd fpgetround (void);
fp_rnd fpsetround (fp_rnd);
typedef int fp_except;
fp_except fpgetmask (void);
fp_except fpsetmask (fp_except);
fp_except fpgetsticky (void);
fp_except fpsetsticky (fp_except);
typedef int fp_rdi;
fp_rdi fpgetroundtoi (void);
fp_rdi fpsetroundtoi (fp_rdi);
int isnan (double);
int isinf (double);
int finite (double);
int isnanf (float);
int isinff (float);
int finitef (float);
union __dmath
{
  double d;
  __ULong i[2];
};
union __fmath
{
  float f;
  __ULong i[1];
};
union __ldmath
{
  long double ld;
  __ULong i[4];
};
extern double atan (double);
extern double cos (double);
extern double sin (double);
extern double tan (double);
extern double tanh (double);
extern double frexp (double, int *);
extern double modf (double, double *);
extern double ceil (double);
extern double fabs (double);
extern double floor (double);
extern double acos (double);
extern double asin (double);
extern double atan2 (double, double);
extern double cosh (double);
extern double sinh (double);
extern double exp (double);
extern double ldexp (double, int);
extern double log (double);
extern double log10 (double);
extern double pow (double, double);
extern double sqrt (double);
extern double fmod (double, double);
    typedef float float_t;
    typedef double double_t;
extern int __isinff (float x);
extern int __isinfd (double x);
extern int __isnanf (float x);
extern int __isnand (double x);
extern int __fpclassifyf (float x);
extern int __fpclassifyd (double x);
extern int __signbitf (float x);
extern int __signbitd (double x);
extern double infinity (void);
extern double nan (const char *);
extern int finite (double);
extern double copysign (double, double);
extern double logb (double);
extern int ilogb (double);
extern double asinh (double);
extern double cbrt (double);
extern double nextafter (double, double);
extern double rint (double);
extern double scalbn (double, int);
extern double exp2 (double);
extern double scalbln (double, long int);
extern double tgamma (double);
extern double nearbyint (double);
extern long int lrint (double);
extern long long int llrint (double);
extern double round (double);
extern long int lround (double);
extern long long int llround (double);
extern double trunc (double);
extern double remquo (double, double, int *);
extern double fdim (double, double);
extern double fmax (double, double);
extern double fmin (double, double);
extern double fma (double, double, double);
extern double log1p (double);
extern double expm1 (double);
extern double acosh (double);
extern double atanh (double);
extern double remainder (double, double);
extern double gamma (double);
extern double lgamma (double);
extern double erf (double);
extern double erfc (double);
extern double log2 (double);
extern double hypot (double, double);
extern float atanf (float);
extern float cosf (float);
extern float sinf (float);
extern float tanf (float);
extern float tanhf (float);
extern float frexpf (float, int *);
extern float modff (float, float *);
extern float ceilf (float);
extern float fabsf (float);
extern float floorf (float);
extern float acosf (float);
extern float asinf (float);
extern float atan2f (float, float);
extern float coshf (float);
extern float sinhf (float);
extern float expf (float);
extern float ldexpf (float, int);
extern float logf (float);
extern float log10f (float);
extern float powf (float, float);
extern float sqrtf (float);
extern float fmodf (float, float);
extern float exp2f (float);
extern float scalblnf (float, long int);
extern float tgammaf (float);
extern float nearbyintf (float);
extern long int lrintf (float);
extern long long llrintf (float);
extern float roundf (float);
extern long int lroundf (float);
extern long long int llroundf (float);
extern float truncf (float);
extern float remquof (float, float, int *);
extern float fdimf (float, float);
extern float fmaxf (float, float);
extern float fminf (float, float);
extern float fmaf (float, float, float);
extern float infinityf (void);
extern float nanf (const char *);
extern int finitef (float);
extern float copysignf (float, float);
extern float logbf (float);
extern int ilogbf (float);
extern float asinhf (float);
extern float cbrtf (float);
extern float nextafterf (float, float);
extern float rintf (float);
extern float scalbnf (float, int);
extern float log1pf (float);
extern float expm1f (float);
extern float acoshf (float);
extern float atanhf (float);
extern float remainderf (float, float);
extern float gammaf (float);
extern float lgammaf (float);
extern float erff (float);
extern float erfcf (float);
extern float log2f (float);
extern float hypotf (float, float);
extern long double atanl (long double);
extern long double cosl (long double);
extern long double sinl (long double);
extern long double tanl (long double);
extern long double tanhl (long double);
extern long double frexpl (long double value, int *);
extern long double modfl (long double, long double *);
extern long double ceill (long double);
extern long double fabsl (long double);
extern long double floorl (long double);
extern long double log1pl (long double);
extern long double expm1l (long double);
extern long double acosl (long double);
extern long double asinl (long double);
extern long double atan2l (long double, long double);
extern long double coshl (long double);
extern long double sinhl (long double);
extern long double expl (long double);
extern long double ldexpl (long double, int);
extern long double logl (long double);
extern long double log10l (long double);
extern long double powl (long double, long double);
extern long double sqrtl (long double);
extern long double fmodl (long double, long double);
extern long double hypotl (long double, long double);
extern long double copysignl (long double, long double);
extern long double nanl (const char *);
extern int ilogbl (long double);
extern long double asinhl (long double);
extern long double cbrtl (long double);
extern long double nextafterl (long double, long double);
extern long double rintl (long double);
extern long double scalbnl (long double, int);
extern long double exp2l (long double);
extern long double scalblnl (long double, long);
extern long double tgammal (long double);
extern long double nearbyintl (long double);
extern long int lrintl (long double);
extern long long int llrintl (long double);
extern long double roundl (long double);
extern long lroundl (long double);
extern long long int llroundl (long double);
extern long double truncl (long double);
extern long double remquol (long double, long double, int *);
extern long double fdiml (long double, long double);
extern long double fmaxl (long double, long double);
extern long double fminl (long double, long double);
extern long double fmal (long double, long double, long double);
extern long double acoshl (long double);
extern long double atanhl (long double);
extern long double remainderl (long double, long double);
extern long double lgammal (long double);
extern long double erfl (long double);
extern long double erfcl (long double);
extern double drem (double, double);
extern void sincos (double, double *, double *);
extern double gamma_r (double, int *);
extern double lgamma_r (double, int *);
extern double y0 (double);
extern double y1 (double);
extern double yn (int, double);
extern double j0 (double);
extern double j1 (double);
extern double jn (int, double);
extern float dremf (float, float);
extern void sincosf (float, float *, float *);
extern float gammaf_r (float, int *);
extern float lgammaf_r (float, int *);
extern float y0f (float);
extern float y1f (float);
extern float ynf (int, float);
extern float j0f (float);
extern float j1f (float);
extern float jnf (int, float);
extern double exp10 (double);
extern double pow10 (double);
extern float exp10f (float);
extern float pow10f (float);
extern int *__signgam (void);
struct exception
{
  int type;
  char *name;
  double arg1;
  double arg2;
  double retval;
  int err;
};
extern int matherr (struct exception *e);
enum __fdlibm_version
{
  __fdlibm_ieee = -1,
  __fdlibm_svid,
  __fdlibm_xopen,
  __fdlibm_posix
};
extern enum __fdlibm_version __fdlib_version;
typedef int error_t;
extern int *__errno (void);
extern const char * const _sys_errlist[];
extern int _sys_nerr;
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned long clock_t;
typedef long time_t;
struct timespec {
  time_t tv_sec;
  long tv_nsec;
};
struct itimerspec {
  struct timespec it_interval;
  struct timespec it_value;
};
typedef long daddr_t;
typedef char * caddr_t;
typedef unsigned short ino_t;
typedef _off_t off_t;
typedef __dev_t dev_t;
typedef __uid_t uid_t;
typedef __gid_t gid_t;
typedef int pid_t;
typedef long key_t;
typedef _ssize_t ssize_t;
typedef unsigned int mode_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned short nlink_t;
typedef long fd_mask;
typedef struct _types_fd_set {
 fd_mask fds_bits[(((64)+(((sizeof (fd_mask) * 8))-1))/((sizeof (fd_mask) * 8)))];
} _types_fd_set;
typedef unsigned long clockid_t;
typedef unsigned long timer_t;
typedef unsigned long useconds_t;
typedef long suseconds_t;
union double_union
{
  double d;
  __uint32_t i[2];
};
typedef __int32_t Long;
typedef union { double d; __ULong i[2]; } U;
typedef struct _Bigint _Bigint;
struct _reent ;
struct FPI;
double __ulp (double x);
double __b2d (_Bigint *a , int *e);
_Bigint * _Balloc (struct _reent *p, int k);
void _Bfree (struct _reent *p, _Bigint *v);
_Bigint * __multadd (struct _reent *p, _Bigint *, int, int);
_Bigint * __s2b (struct _reent *, const char*, int, int, __ULong);
_Bigint * __i2b (struct _reent *,int);
_Bigint * __multiply (struct _reent *, _Bigint *, _Bigint *);
_Bigint * __pow5mult (struct _reent *, _Bigint *, int k);
int __hi0bits (__ULong);
int __lo0bits (__ULong *);
_Bigint * __d2b (struct _reent *p, double d, int *e, int *bits);
_Bigint * __lshift (struct _reent *p, _Bigint *b, int k);
_Bigint * __mdiff (struct _reent *p, _Bigint *a, _Bigint *b);
int __mcmp (_Bigint *a, _Bigint *b);
int __gethex (struct _reent *p, const char **sp, const struct FPI *fpi, Long *exp, _Bigint **bp, int sign);
double __ratio (_Bigint *a, _Bigint *b);
__ULong __any_on (_Bigint *b, int k);
void __copybits (__ULong *c, int n, _Bigint *b);
int __hexnan (const char **sp, const struct FPI *fpi, __ULong *x0);
extern const double __mprec_tinytens[];
extern const double __mprec_bigtens[];
extern const double __mprec_tens[];
extern const unsigned char __hexdig[];
double _mprec_log10 (int);
static int
quorem(_Bigint * b , _Bigint * S)
{
  int n;
  long borrow, y;
  __ULong carry, q, ys;
  __ULong *bx, *bxe, *sx, *sxe;
  long z;
  __ULong si, zs;
  n = S->_wds;
  if (b->_wds < n)
    return 0;
  sx = S->_x;
  sxe = sx + --n;
  bx = b->_x;
  bxe = bx + n;
  q = *bxe / (*sxe + 1);
  if (q)
    {
      borrow = 0;
      carry = 0;
      do
 {
   si = *sx++;
   ys = (si & 0xffff) * q + carry;
   zs = (si >> 16) * q + (ys >> 16);
   carry = zs >> 16;
   y = (*bx & 0xffff) - (ys & 0xffff) + borrow;
   borrow = y >> 16;
   ;
   z = (*bx >> 16) - (zs & 0xffff) + borrow;
   borrow = z >> 16;
   ;
   (*(bx)++ = ((z) << 16) | ((y) & 0xffff));
 }
      while (sx <= sxe);
      if (!*bxe)
 {
   bx = b->_x;
   while (--bxe > bx && !*bxe)
     --n;
   b->_wds = n;
 }
    }
  if (__mcmp (b, S) >= 0)
    {
      q++;
      borrow = 0;
      carry = 0;
      bx = b->_x;
      sx = S->_x;
      do
 {
   si = *sx++;
   ys = (si & 0xffff) + carry;
   zs = (si >> 16) + (ys >> 16);
   carry = zs >> 16;
   y = (*bx & 0xffff) - (ys & 0xffff) + borrow;
   borrow = y >> 16;
   ;
   z = (*bx >> 16) - (zs & 0xffff) + borrow;
   borrow = z >> 16;
   ;
   (*(bx)++ = ((z) << 16) | ((y) & 0xffff));
 }
      while (sx <= sxe);
      bx = b->_x;
      bxe = bx + n;
      if (!*bxe)
 {
   while (--bxe > bx && !*bxe)
     --n;
   b->_wds = n;
 }
    }
  return q;
}
char *
_dtoa_r(struct _reent *ptr , double _d , int mode , int ndigits , int *decpt , int *sign , char **rve)
{
  int bbits, b2, b5, be, dig, i, ieps, ilim, ilim0, ilim1, j, j1, k, k0,
    k_check, leftright, m2, m5, s2, s5, spec_case, try_quick;
  union double_union d, d2, eps;
  long L;
  int denorm;
  __ULong x;
  _Bigint *b, *b1, *delta, *mlo = ((void *)0), *mhi, *S;
  double ds;
  char *s, *s0;
  d.d = _d;
  ;
  if (((ptr)->_result))
    {
      ((ptr)->_result)->_k = ((ptr)->_result_k);
      ((ptr)->_result)->_maxwds = 1 << ((ptr)->_result_k);
      _Bfree (ptr, ((ptr)->_result));
      ((ptr)->_result) = 0;
    }
  if ((d.i[1]) & ((__uint32_t)0x80000000L))
    {
      *sign = 1;
      (d.i[1]) &= ~((__uint32_t)0x80000000L);
    }
  else
    *sign = 0;
  if (((d.i[1]) & ((__uint32_t)0x7ff00000L)) == ((__uint32_t)0x7ff00000L))
    {
      *decpt = 9999;
      s =
 !(d.i[0]) && !((d.i[1]) & 0xfffff) ? "Infinity" :
 "NaN";
      if (rve)
 *rve =
   s[3] ? s + 8 :
   s + 3;
      return s;
    }
  if (!d.d)
    {
      *decpt = 1;
      s = "0";
      if (rve)
 *rve = s + 1;
      return s;
    }
  b = __d2b (ptr, d.d, &be, &bbits);
  if ((i = (int) ((d.i[1]) >> 20 & (((__uint32_t)0x7ff00000L) >> 20))) != 0)
    {
      d2.d = d.d;
      (d2.i[1]) &= ((__uint32_t)0xfffffL);
      (d2.i[1]) |= ((__uint32_t)0x3ff00000L);
      i -= 1023;
      denorm = 0;
    }
  else
    {
      i = bbits + be + (1023 + (53 - 1) - 1);
      x = (i > 32) ? ((d.i[1]) << (64 - i)) | ((d.i[0]) >> (i - 32))
       : ((d.i[0]) << (32 - i));
      d2.d = x;
      (d2.i[1]) -= 31 * ((__uint32_t)0x100000L);
      i -= (1023 + (53 - 1) - 1) + 1;
      denorm = 1;
    }
  ds = (d2.d - 1.5) * 0.289529654602168 + 0.1760912590558 + i * 0.301029995663981;
  k = (int) ds;
  if (ds < 0. && ds != k)
    k--;
  k_check = 1;
  if (k >= 0 && k <= 22)
    {
      if (d.d < __mprec_tens[k])
 k--;
      k_check = 0;
    }
  j = bbits - i - 1;
  if (j >= 0)
    {
      b2 = 0;
      s2 = j;
    }
  else
    {
      b2 = -j;
      s2 = 0;
    }
  if (k >= 0)
    {
      b5 = 0;
      s5 = k;
      s2 += k;
    }
  else
    {
      b2 -= k;
      b5 = -k;
      s5 = 0;
    }
  if (mode < 0 || mode > 9)
    mode = 0;
  try_quick = 1;
  if (mode > 5)
    {
      mode -= 4;
      try_quick = 0;
    }
  leftright = 1;
  ilim = ilim1 = -1;
  switch (mode)
    {
    case 0:
    case 1:
      i = 18;
      ndigits = 0;
      break;
    case 2:
      leftright = 0;
    case 4:
      if (ndigits <= 0)
 ndigits = 1;
      ilim = ilim1 = i = ndigits;
      break;
    case 3:
      leftright = 0;
    case 5:
      i = ndigits + k + 1;
      ilim = i;
      ilim1 = i - 1;
      if (i <= 0)
 i = 1;
    }
  j = sizeof (__ULong);
  for (((ptr)->_result_k) = 0; sizeof (_Bigint) - sizeof (__ULong) + j <= i;
       j <<= 1)
    ((ptr)->_result_k)++;
  ((ptr)->_result) = _Balloc (ptr, ((ptr)->_result_k));
  s = s0 = (char *) ((ptr)->_result);
  if (ilim >= 0 && ilim <= 14 && try_quick)
    {
      i = 0;
      d2.d = d.d;
      k0 = k;
      ilim0 = ilim;
      ieps = 2;
      if (k > 0)
 {
   ds = __mprec_tens[k & 0xf];
   j = k >> 4;
   if (j & 0x10)
     {
       j &= 0x10 - 1;
       d.d /= __mprec_bigtens[5 - 1];
       ieps++;
     }
   for (; j; j >>= 1, i++)
     if (j & 1)
       {
  ieps++;
  ds *= __mprec_bigtens[i];
       }
   d.d /= ds;
 }
      else if ((j1 = -k) != 0)
 {
   d.d *= __mprec_tens[j1 & 0xf];
   for (j = j1 >> 4; j; j >>= 1, i++)
     if (j & 1)
       {
  ieps++;
  d.d *= __mprec_bigtens[i];
       }
 }
      if (k_check && d.d < 1. && ilim > 0)
 {
   if (ilim1 <= 0)
     goto fast_failed;
   ilim = ilim1;
   k--;
   d.d *= 10.;
   ieps++;
 }
      eps.d = ieps * d.d + 7.;
      (eps.i[1]) -= (53 - 1) * ((__uint32_t)0x100000L);
      if (ilim == 0)
 {
   S = mhi = 0;
   d.d -= 5.;
   if (d.d > eps.d)
     goto one_digit;
   if (d.d < -eps.d)
     goto no_digits;
   goto fast_failed;
 }
      if (leftright)
 {
   eps.d = 0.5 / __mprec_tens[ilim - 1] - eps.d;
   for (i = 0;;)
     {
       L = d.d;
       d.d -= L;
       *s++ = '0' + (int) L;
       if (d.d < eps.d)
  goto ret1;
       if (1. - d.d < eps.d)
  goto bump_up;
       if (++i >= ilim)
  break;
       eps.d *= 10.;
       d.d *= 10.;
     }
 }
      else
 {
   eps.d *= __mprec_tens[ilim - 1];
   for (i = 1;; i++, d.d *= 10.)
     {
       L = d.d;
       d.d -= L;
       *s++ = '0' + (int) L;
       if (i == ilim)
  {
    if (d.d > 0.5 + eps.d)
      goto bump_up;
    else if (d.d < 0.5 - eps.d)
      {
        while (*--s == '0');
        s++;
        goto ret1;
      }
    break;
  }
     }
 }
    fast_failed:
      s = s0;
      d.d = d2.d;
      k = k0;
      ilim = ilim0;
    }
  if (be >= 0 && k <= 14)
    {
      ds = __mprec_tens[k];
      if (ndigits < 0 && ilim <= 0)
 {
   S = mhi = 0;
   if (ilim < 0 || d.d <= 5 * ds)
     goto no_digits;
   goto one_digit;
 }
      for (i = 1;; i++)
 {
   L = d.d / ds;
   d.d -= L * ds;
   *s++ = '0' + (int) L;
   if (i == ilim)
     {
       d.d += d.d;
             if ((d.d > ds) || ((d.d == ds) && (L & 1)))
  {
  bump_up:
    while (*--s == '9')
      if (s == s0)
        {
   k++;
   *s = '0';
   break;
        }
    ++*s++;
  }
       break;
     }
   if (!(d.d *= 10.))
     break;
 }
      goto ret1;
    }
  m2 = b2;
  m5 = b5;
  mhi = mlo = 0;
  if (leftright)
    {
      if (mode < 2)
 {
   i =
     denorm ? be + (1023 + (53 - 1) - 1 + 1) :
     1 + 53 - bbits;
 }
      else
 {
   j = ilim - 1;
   if (m5 >= j)
     m5 -= j;
   else
     {
       s5 += j -= m5;
       b5 += j;
       m5 = 0;
     }
   if ((i = ilim) < 0)
     {
       m2 -= i;
       i = 0;
     }
 }
      b2 += i;
      s2 += i;
      mhi = __i2b (ptr, 1);
    }
  if (m2 > 0 && s2 > 0)
    {
      i = m2 < s2 ? m2 : s2;
      b2 -= i;
      m2 -= i;
      s2 -= i;
    }
  if (b5 > 0)
    {
      if (leftright)
 {
   if (m5 > 0)
     {
       mhi = __pow5mult (ptr, mhi, m5);
       b1 = __multiply (ptr, mhi, b);
       _Bfree (ptr, b);
       b = b1;
     }
         if ((j = b5 - m5) != 0)
     b = __pow5mult (ptr, b, j);
 }
      else
 b = __pow5mult (ptr, b, b5);
    }
  S = __i2b (ptr, 1);
  if (s5 > 0)
    S = __pow5mult (ptr, S, s5);
  spec_case = 0;
  if (mode < 2)
    {
      if (!(d.i[0]) && !((d.i[1]) & ((__uint32_t)0xfffffL))
   && (d.i[1]) & ((__uint32_t)0x7ff00000L)
 )
 {
   b2 += 1;
   s2 += 1;
   spec_case = 1;
 }
    }
  if ((i = ((s5 ? 32 - __hi0bits (S->_x[S->_wds - 1]) : 1) + s2) & 0x1f) != 0)
    i = 32 - i;
  if (i > 4)
    {
      i -= 4;
      b2 += i;
      m2 += i;
      s2 += i;
    }
  else if (i < 4)
    {
      i += 28;
      b2 += i;
      m2 += i;
      s2 += i;
    }
  if (b2 > 0)
    b = __lshift (ptr, b, b2);
  if (s2 > 0)
    S = __lshift (ptr, S, s2);
  if (k_check)
    {
      if (__mcmp (b, S) < 0)
 {
   k--;
   b = __multadd (ptr, b, 10, 0);
   if (leftright)
     mhi = __multadd (ptr, mhi, 10, 0);
   ilim = ilim1;
 }
    }
  if (ilim <= 0 && mode > 2)
    {
      if (ilim < 0 || __mcmp (b, S = __multadd (ptr, S, 5, 0)) <= 0)
 {
 no_digits:
   k = -1 - ndigits;
   goto ret;
 }
    one_digit:
      *s++ = '1';
      k++;
      goto ret;
    }
  if (leftright)
    {
      if (m2 > 0)
 mhi = __lshift (ptr, mhi, m2);
      mlo = mhi;
      if (spec_case)
 {
   mhi = _Balloc (ptr, mhi->_k);
   memcpy((char *)&mhi->_sign, (char *)&mlo->_sign, mlo->_wds*sizeof(long) + 2*sizeof(int));
   mhi = __lshift (ptr, mhi, 1);
 }
      for (i = 1;; i++)
 {
   dig = quorem (b, S) + '0';
   j = __mcmp (b, mlo);
   delta = __mdiff (ptr, S, mhi);
   j1 = delta->_sign ? 1 : __mcmp (b, delta);
   _Bfree (ptr, delta);
   if (j1 == 0 && !mode && !((d.i[0]) & 1))
     {
       if (dig == '9')
  goto round_9_up;
       if (j > 0)
  dig++;
       *s++ = dig;
       goto ret;
     }
         if ((j < 0) || ((j == 0) && !mode
       && !((d.i[0]) & 1)
           ))
     {
       if (j1 > 0)
  {
    b = __lshift (ptr, b, 1);
    j1 = __mcmp (b, S);
                 if (((j1 > 0) || ((j1 == 0) && (dig & 1)))
        && dig++ == '9')
      goto round_9_up;
  }
       *s++ = dig;
       goto ret;
     }
   if (j1 > 0)
     {
       if (dig == '9')
  {
  round_9_up:
    *s++ = '9';
    goto roundoff;
  }
       *s++ = dig + 1;
       goto ret;
     }
   *s++ = dig;
   if (i == ilim)
     break;
   b = __multadd (ptr, b, 10, 0);
   if (mlo == mhi)
     mlo = mhi = __multadd (ptr, mhi, 10, 0);
   else
     {
       mlo = __multadd (ptr, mlo, 10, 0);
       mhi = __multadd (ptr, mhi, 10, 0);
     }
 }
    }
  else
    for (i = 1;; i++)
      {
 *s++ = dig = quorem (b, S) + '0';
 if (i >= ilim)
   break;
 b = __multadd (ptr, b, 10, 0);
      }
  b = __lshift (ptr, b, 1);
  j = __mcmp (b, S);
  if ((j > 0) || ((j == 0) && (dig & 1)))
    {
    roundoff:
      while (*--s == '9')
 if (s == s0)
   {
     k++;
     *s++ = '1';
     goto ret;
   }
      ++*s++;
    }
  else
    {
      while (*--s == '0');
      s++;
    }
ret:
  _Bfree (ptr, S);
  if (mhi)
    {
      if (mlo && mlo != mhi)
 _Bfree (ptr, mlo);
      _Bfree (ptr, mhi);
    }
ret1:
  _Bfree (ptr, b);
  *s = 0;
  *decpt = k + 1;
  if (rve)
    *rve = s;
  return s0;
}
