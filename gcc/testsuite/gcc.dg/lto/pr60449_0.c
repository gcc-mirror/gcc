/* { dg-lto-do link } */
/* { dg-require-effective-target gettimeofday } */

extern int printf (const char *__restrict __format, ...);
typedef long int __time_t;
typedef long int __suseconds_t;

struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };

struct timezone
  {
    int tz_minuteswest;
    int tz_dsttime;
  };
typedef struct timezone *__restrict __timezone_ptr_t;

extern int gettimeofday (struct timeval *__restrict __tv, __timezone_ptr_t __tz);

int bar (void)
{
  struct timeval tv;
  struct timezone tz;

  gettimeofday (&tv, &tz);
  printf ("This is from bar %i\n", tz.tz_dsttime);
  return 5;
}
