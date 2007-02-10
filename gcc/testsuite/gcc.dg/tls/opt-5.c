/* { dg-do compile } */
/* { dg-options "-O2" } */
/* Sched1 moved {load_tp} pattern between strlen call and the copy
   of the hard return value to its pseudo.  This resulted in a
   reload abort, since the hard register was not spillable.  */

extern __thread int __libc_errno __attribute__ ((tls_model ("initial-exec")));

struct stat64
  {
    long dummy[4];
  };
typedef __SIZE_TYPE__ size_t;
typedef unsigned long long uint64_t;
typedef int __mode_t;

extern size_t strlen (__const char *__s) __attribute__ ((__pure__));
extern int strcmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__pure__));

extern int __open64 (__const char *__file, int __oflag, ...);
extern int __open (__const char *__file, int __oflag, ...);
extern int __mkdir (__const char *__path, __mode_t __mode);
extern int __lxstat64 (int __ver, __const char *__filename,
                       struct stat64 *__stat_buf) ;

static const char letters[] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

int
__gen_tempname (char *tmpl, int kind)
{
  int len;
  char *XXXXXX;
  static uint64_t value;
  uint64_t random_time_bits;
  unsigned int count;
  int fd = -1;
  int save_errno = __libc_errno;
  struct stat64 st;
  unsigned int attempts_min = 62 * 62 * 62;
  unsigned int attempts = attempts_min < 238328 ? 238328 : attempts_min;

  len = strlen (tmpl);
  if (len < 6 || strcmp(&tmpl[len - 6], "XXXXXX"))
    {
      (__libc_errno = (22));
      return -1;
    }

  XXXXXX = &tmpl[len - 6];

  for (count = 0; count < attempts; value += 7777, ++count)
    {
      uint64_t v = value;

      XXXXXX[0] = letters[v % 62];
      v /= 62;
      XXXXXX[1] = letters[v % 62];
      v /= 62;
      XXXXXX[2] = letters[v % 62];
      v /= 62;
      XXXXXX[3] = letters[v % 62];
      v /= 62;
      XXXXXX[4] = letters[v % 62];
      v /= 62;
      XXXXXX[5] = letters[v % 62];

      switch (kind)
        {
        case 0:
          fd = __open (tmpl, 02 | 01000 | 04000, 0400 | 0200);
          break;

        case 1:
          fd = __open64 (tmpl, 02 | 01000 | 04000, 0400 | 0200);
          break;

        case 2:
          fd = __mkdir (tmpl, 0400 | 0200 | 0100);
          break;

        case 3:
          if (__lxstat64 (2, tmpl, &st) < 0)
            {
              if (__libc_errno == 2)
                {
                  (__libc_errno = (save_errno));
                  return 0;
                }
              else

                return -1;
            }
          continue;
        }

      if (fd >= 0)
        {
          (__libc_errno = (save_errno));
          return fd;
        }
      else if (__libc_errno != 17)
        return -1;
    }

  (__libc_errno = (17));
  return -1;
}
