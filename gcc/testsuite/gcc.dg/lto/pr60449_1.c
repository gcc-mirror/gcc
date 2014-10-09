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
extern int gettimeofday (struct timeval *__restrict __tv,
    __timezone_ptr_t __tz) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

typedef long int __jmp_buf[8];
typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
struct __jmp_buf_tag
  {
    __jmp_buf __jmpbuf;
    int __mask_was_saved;
    __sigset_t __saved_mask;
  };
typedef struct __jmp_buf_tag jmp_buf[1];

extern int setjmp (jmp_buf __env) __attribute__ ((__nothrow__));
extern void longjmp (struct __jmp_buf_tag __env[1], int __val)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));

extern int bar (void);

int __attribute__ ((noinline, noclone))
get_input (void)
{
  return 0;
}

static jmp_buf buf;

int foo (void)
{
  if (get_input ())
    longjmp(buf, 1);
  return 0;
}

volatile int z;


int main (void)
{
  struct timeval tv;
  struct timezone tz;

  bar();
  if (setjmp (buf))
    return 1;

  if (!get_input ())
    {
      gettimeofday (&tv, &tz);
      z = 0;
      printf ("This is from main %i\n", tz.tz_dsttime);
    }

  foo ();
  bar ();
  bar ();

  return 0;
}
