/* Adapted from gcc.dg/Winfinite-recursion.c.  */

#define NORETURN __attribute__ ((noreturn))

typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern void exit (int);

extern int ei;
int (*pfi_v)(void);


/* Make sure the warning doesn't assume every call has a DECL.  */

int nowarn_pfi_v (void)
{
  return pfi_v ();
}


int warn_fi_v (void)
{
  return warn_fi_v ();               // { dg-warning "-Wanalyzer-infinite-recursion" }
}

/* Verify #pragma suppression works.  */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-infinite-recursion"

int suppress_warn_fi_v (void)
{
  return suppress_warn_fi_v ();
}

#pragma GCC diagnostic pop


int nowarn_fi_v (void)
{
  if (ei++ == 0)
    return nowarn_fi_v ();
  return 0;
}

/* -Winfinite-recursion warns for this, but
   -Wanalyzer-infinite-recursion doesn't.  */

int warn_if_i (int i)
{
  if (i > 0)
    return warn_if_i (--i);
  else if (i < 0)
    return warn_if_i (-i);
  else
    return warn_if_i (7);
}


int nowarn_if_i (int i)
{
  if (i > 0)
    return nowarn_if_i (--i);
  else if (i < 0)
    return nowarn_if_i (-i);
  else
    return -1;
}

int nowarn_switch (int i, int a[])
{
  switch (i)
    {
    case 0: return nowarn_switch (a[3], a + 1);
    case 1: return nowarn_switch (a[5], a + 2);
    case 2: return nowarn_switch (a[7], a + 3);
    case 3: return nowarn_switch (a[9], a + 4);
    }
  return 77;
}

/* -Winfinite-recursion warns for this, but
   -Wanalyzer-infinite-recursion doesn't.  */

int warn_switch (int i, int a[])
{
  switch (i)
    {
    case 0: return warn_switch (a[3], a + 1);
    case 1: return warn_switch (a[5], a + 2);
    case 2: return warn_switch (a[7], a + 3);
    case 3: return warn_switch (a[9], a + 4);
    default: return warn_switch (a[1], a + 5);
    }
}

NORETURN void fnoreturn (void);

/* Verify there's no warning for a function that doesn't return.  */
int nowarn_call_noret (void)
{
  fnoreturn ();
}

int warn_call_noret_r (void)
{
  warn_call_noret_r ();             // { dg-warning "-Wanalyzer-infinite-recursion" }
  fnoreturn ();
}

/* -Winfinite-recursion warns for this, but
   -Wanalyzer-infinite-recursion doesn't.  */

int
warn_noret_call_abort_r (char *s, int n)
{
  if (!s)
    abort ();

  if (n > 7)
    abort ();

  return n + warn_noret_call_abort_r (s, n - 1);
}

NORETURN void nowarn_noret_call_abort_r (int n)
{
  if (n > 7)
    abort ();

  nowarn_noret_call_abort_r (n - 1);
}

/* -Winfinite-recursion warns for this, but
   -Wanalyzer-infinite-recursion doesn't.  */

int warn_call_abort_r (int n)
{
  n += warn_call_abort_r (n - 1);
  if (n > 7)   // unreachable
    abort ();
  return n;
}


/* -Winfinite-recursion warns for this, but
   -Wanalyzer-infinite-recursion doesn't.  */

int warn_call_exit_r (int n)
{
  n += warn_call_exit_r (n - 1);
  if (n > 7)
    exit (0);
  return n;
}

struct __jmp_buf_tag { };
typedef struct __jmp_buf_tag jmp_buf[1];

extern jmp_buf jmpbuf;

/* A call to longjmp() breaks infinite recursion.  Verify it suppresses
   the warning.  */

int nowarn_call_longjmp_r (int n)
{
  if (n > 7)
    __builtin_longjmp (jmpbuf, 1);
  return n + nowarn_call_longjmp_r (n - 1);
}

/* -Winfinite-recursion warns for this, but
   -Wanalyzer-infinite-recursion doesn't.  */

int warn_call_longjmp_r (int n)
{
  n += warn_call_longjmp_r (n - 1);
  if (n > 7)
    __builtin_longjmp (jmpbuf, 1);
  return n;
}


struct __sigjmp_buf_tag { };
typedef struct __sigjmp_buf_tag sigjmp_buf[1];

extern sigjmp_buf sigjmpbuf;

/* GCC has no __builtin_siglongjmp().  */
extern void siglongjmp (sigjmp_buf, int);

/* A call to longjmp() breaks infinite recursion.  Verify it suppresses
   the warning.  */

int nowarn_call_siglongjmp_r (int n)
{
  if (n > 7)
    siglongjmp (sigjmpbuf, 1);
  return n + nowarn_call_siglongjmp_r (n - 1);
}

/* -Winfinite-recursion doesn't warn for this unbounded recursion, but
   -Wanalyzer-infinite-recursion does.  */

int nowarn_while_do_call_r (int n)
{
  int z = 0;
  while (n)
    z += nowarn_while_do_call_r (n--); // { dg-warning "-Wanalyzer-infinite-recursion" }
  return z;
}

int warn_do_while_call_r (int n)
{
  int z = 0;
  do
    z += warn_do_while_call_r (n); // { dg-warning "-Wanalyzer-infinite-recursion" }
  while (--n);
  return z;
}
