/* PR tree-optimization/77671 - missing -Wformat-overflow warning
   on sprintf overflow with "%s"
   { dg-do compile }
   { dg-options "-O2 -Wformat -Wno-format-zero-length -fdump-tree-optimized" } */

void sink (char*);

extern char buffer[];

/* String exactly 4100 characters long (plus the terminating NUL).  */
extern const char s4100[4101];

void test_sprintf (const char *s)
{
#define IGN(...) __builtin_sprintf (buffer, __VA_ARGS__); sink (buffer)

  /* Each of the following calls is expected to be transformed into
     one of memcpy or strcpy.  */
  IGN ("");
  IGN ("a");
  IGN ("ab");
  /* FIXME: Transform to strcpy/memcpy.  */
  /* IGN (s4100 + 5); */

  IGN ("%s", "");
  IGN ("%s", "a");
  IGN ("%s", "ab");

  IGN ("%s", s4100 + 5);

  /* FIXME: This can be transformed into strcpy.  */
  /* IGN (s); */
  IGN ("%s", s);
}


void test_snprintf (void)
{
#undef IGN
#define IGN(N, ...) __builtin_snprintf (buffer, N, __VA_ARGS__); sink (buffer)

  /* Each of the following calls is expected to be transformed into
     one of memcpy or strcpy.  */
  IGN (1, "");
  IGN (2, "1");
  IGN (8, "1234567");

  /* FIXME: Transform to strcpy/memcpy.  */
  /* IGN (4096, s4100 + 5); */

  IGN (1, "%s", "");
  IGN (2, "%s", "1");
  IGN (8, "%s", "1234567");

  IGN (4096, "%s", s4100 + 5);
}

#if 0   /* FIXME: Implement vs{,n}printf optimization.  */

void test_vsprintf (__builtin_va_list va)
{
#undef IGN
#define IGN(fmt) __builtin_vsprintf (buffer, fmt, va); sink (buffer)

  /* Each of the following calls is expected to be transformed into
     one of memcpy or strcpy.  */
  IGN ("");
  IGN ("a");
  IGN ("ab");
  IGN (s4100 + 5);

  IGN ("%s");
}

void test_vsnprintf (__builtin_va_list va)
{
#undef IGN
#define IGN(N, fmt) __builtin_vsnprintf (buffer, N, fmt, va); sink (buffer)

  /* Each of the following calls is expected to be transformed into
     one of memcpy or strcpy.  */
  IGN (   1, "");
  IGN (   2, "1");
  IGN (   8, "1234567");
  IGN (4096, s4100 + 5);
}

#endif

/* { dg-final { scan-tree-dump-not "builtin_sprintf" "optimized" } }
   { dg-final { scan-tree-dump-not "builtin_snprintf" "optimized" } }
   { dg-final { scan-tree-dump-not "builtin_vsprintf" "optimized" } }
   { dg-final { scan-tree-dump-not "builtin_vsnprintf" "optimized" } } */

#define S10    "0123456789"
#define S100   S10 S10 S10 S10 S10  S10 S10 S10 S10 S10
#define S1000  S100 S100 S100 S100 S100  S100 S100 S100 S100 S100

const char s4100[4101] = S1000 S1000 S1000 S1000 S100;
