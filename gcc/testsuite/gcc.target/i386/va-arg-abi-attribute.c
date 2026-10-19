/* ix86_gimplify_va_arg must decide whether an argument is passed by
   reference from the va_list type, not from the ABI of the function
   containing the va_arg or from the target default.  */
/* { dg-do run { target { lp64 || llp64 } } } */
/* { dg-options "-std=gnu99" } */

typedef int v4si __attribute__ ((vector_size (16)));
struct s3 { char a, b, c; };
struct s8 { int a, b; };
struct s12 { int a, b, c; };
struct s16 { long long a, b; };

#define READER(name, abi, list)						\
static __attribute__ ((abi, noipa)) void				\
name (list ap)								\
{									\
  struct s3 x3 = __builtin_va_arg (ap, struct s3);			\
  struct s8 x8 = __builtin_va_arg (ap, struct s8);			\
  struct s12 x12 = __builtin_va_arg (ap, struct s12);			\
  struct s16 x16 = __builtin_va_arg (ap, struct s16);			\
  v4si v = __builtin_va_arg (ap, v4si);					\
  int i = __builtin_va_arg (ap, int);					\
  if (x3.a != 1 || x3.b != 2 || x3.c != 3				\
      || x8.a != 4 || x8.b != 5						\
      || x12.a != 6 || x12.b != 7 || x12.c != 8				\
      || x16.a != 9 || x16.b != 10					\
      || v[0] != 11 || v[1] != 12 || v[2] != 13 || v[3] != 14		\
      || i != 15)							\
    __builtin_abort ();							\
}

READER (sysv_reads_sysv, sysv_abi, __builtin_sysv_va_list)
READER (ms_reads_sysv, ms_abi, __builtin_sysv_va_list)
READER (sysv_reads_ms, sysv_abi, __builtin_ms_va_list)
READER (ms_reads_ms, ms_abi, __builtin_ms_va_list)

static __attribute__ ((sysv_abi, noipa)) void
sysv_varargs (int n, ...)
{
  __builtin_sysv_va_list ap;
  __builtin_sysv_va_start (ap, n);
  sysv_reads_sysv (ap);
  __builtin_sysv_va_end (ap);
  __builtin_sysv_va_start (ap, n);
  ms_reads_sysv (ap);
  __builtin_sysv_va_end (ap);
}

static __attribute__ ((ms_abi, noipa)) void
ms_varargs (int n, ...)
{
  __builtin_ms_va_list ap;
  __builtin_ms_va_start (ap, n);
  sysv_reads_ms (ap);
  __builtin_ms_va_end (ap);
  __builtin_ms_va_start (ap, n);
  ms_reads_ms (ap);
  __builtin_ms_va_end (ap);
}

int
main (void)
{
  struct s3 x3 = { 1, 2, 3 };
  struct s8 x8 = { 4, 5 };
  struct s12 x12 = { 6, 7, 8 };
  struct s16 x16 = { 9, 10 };
  v4si v = { 11, 12, 13, 14 };

  sysv_varargs (0, x3, x8, x12, x16, v, 15);
  ms_varargs (0, x3, x8, x12, x16, v, 15);
  return 0;
}
