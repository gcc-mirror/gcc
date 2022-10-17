/* PR middle-end/88232 - Please implement -Winfinite-recursion
   Exercise warning with optimization.  Same as -Winfinite-recursion.c
   plus mutually recursive calls that depend on inlining.
   { dg-do compile }
   { dg-options "-O2 -Wall -Winfinite-recursion" } */

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


int warn_fi_v (void)                // { dg-warning "-Winfinite-recursion" }
{
  return warn_fi_v ();              // { dg-message "recursive call" }
}

/* Verify #pragma suppression works.  */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Winfinite-recursion"

int suppress_warn_fi_v (void)
{
  return warn_fi_v ();
}

#pragma GCC diagnostic pop

int nowarn_fi_v (void)
{
  if (ei++ == 0)
    return nowarn_fi_v ();
  return 0;
}


int warn_if_i (int i)               // { dg-warning "-Winfinite-recursion" }
{
  if (i > 0)
    return warn_if_i (--i);         // { dg-message "recursive call" }
  else if (i < 0)
    return warn_if_i (-i);          // { dg-message "recursive call" }
  else
    return warn_if_i (7);           // { dg-message "recursive call" }
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

int warn_switch (int i, int a[])    // { dg-warning "-Winfinite-recursion" }
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

int warn_call_noret_r (void)        // { dg-warning "-Winfinite-recursion" }
{
  warn_call_noret_r ();             // { dg-message "recursive call" }
  fnoreturn ();
}

/* Verify a warning even though the abort() call would prevent the infinite
   recursion.  There's no good way to tell the two cases apart and letting
   a simple abort prevent the warning would make it ineffective in cases
   where it's the result of assert() expansion and not meant to actually
   prevent recursion.  */

int
warn_noret_call_abort_r (char *s, int n)  // { dg-warning "-Winfinite-recursion" }
{
  if (!s)
    abort ();

  if (n > 7)
    abort ();

  return n + warn_noret_call_abort_r (s, n - 1);  // { dg-message "recursive call" }
}

/* Verify that a warning is not issued for an apparently infinitely
   recursive function like the one above where the recursion would be
   prevented by a call to a noreturn function if the recursive function
   is itself declared noreturn.  */

NORETURN void nowarn_noret_call_abort_r (int n)
{
  if (n > 7)
    abort ();

  nowarn_noret_call_abort_r (n - 1);
}

int warn_call_abort_r (int n)       // { dg-warning "-Winfinite-recursion" }
{
  n += warn_call_abort_r (n - 1);   // { dg-message "recursive call" }
  if (n > 7)   // unreachable
    abort ();
  return n;
}


/* And again with exit() for good measure.  */

int warn_call_exit_r (int n)        // { dg-warning "-Winfinite-recursion" }
{
  n += warn_call_exit_r (n - 1);    // { dg-message "recursive call" }
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

int warn_call_longjmp_r (int n)     // { dg-warning "-Winfinite-recursion" }
{
  n += warn_call_longjmp_r (n - 1); // { dg-message "recursive call" }
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


int nowarn_while_do_call_r (int n)
{
  int z = 0;
  while (n)
    z += nowarn_while_do_call_r (n--);
  return z;
}

int warn_do_while_call_r (int n)    // { dg-warning "-Winfinite-recursion" }
{
  int z = 0;
  do
    z += warn_do_while_call_r (n);  // { dg-message "recursive call" }
  while (--n);
  return z;
}


/* Verify warnings for a naive replacement of a built-in fucntion.  */

void* malloc (size_t n)             // { dg-warning "-Winfinite-recursion" }
{
  size_t *p =
    (size_t*)__builtin_malloc (n + sizeof n);   // { dg-message "recursive call" }
  *p = n;
  return p + 1;
}


int nowarn_fact (int n)
{
  return n ? n * nowarn_fact (n - 1) : 1;
}


static int fi_v (void);

/* It would seem preferable to issue the warning for the extern function
   but as it happens it's the static function that's inlined into a recursive
   call to itself and warn_call_fi_v() expands to a call to it.  */

int warn_call_fi_v (void)     // { dg-warning "-Winfinite-recursion" "" { xfail *-*-* } }
{
  return fi_v ();             // { dg-message "recursive call" }
}

static int fi_v (void)        // { dg-warning "-Winfinite-recursion" }
{
  return warn_call_fi_v ();
}
