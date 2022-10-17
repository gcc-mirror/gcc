/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fprofile-arcs -fprofile-info-section" } */

#define assert(expr)                                            \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("%s:%i: Assertion `%s' failed.\n",      \
                        __FILE__, __LINE__, #expr),             \
      __builtin_abort ()))

struct gcov_info;

extern void
__gcov_info_to_gcda (const struct gcov_info *__info,
		     void (*__filename_fn) (const char *, void *),
		     void (*__dump_fn) (const void *, unsigned, void *),
		     void *(*__allocate_fn) (unsigned, void *),
		     void *__arg);

extern void
__gcov_filename_to_gcfn (const char *__filename,
			 void (*__dump_fn) (const void *, unsigned, void *),
			 void *__arg);

extern const struct gcov_info *my_info;

static unsigned counter;

static unsigned counter_after_filename;

static int check_zero;

static int check_after_filename;

static void
dump (const void *d, unsigned n, void *arg)
{
  unsigned *m = (unsigned *)arg;
  assert (arg == &counter);

  if (*m == 0)
  {
    const unsigned *u = d;
    assert (*u == 0x6763666e);
    check_zero = 1;
  }
  else if (*m == counter_after_filename)
  {
    const unsigned *u = d;
    assert (*u == 0x67636461);
    check_after_filename = 1;
  }

  *m += n;
}

static void
filename (const char *f, void *arg)
{
  assert (arg == &counter);
  assert (__builtin_strstr (f, "gcov-info-to-gcda.c") == 0);
  __gcov_filename_to_gcfn (f, dump, arg);
  counter_after_filename = counter;
}

static void *
allocate (unsigned length, void *arg)
{
  assert (arg == &counter);
  return __builtin_malloc (length);
}

int main()
{
  __asm__ volatile (".set my_info, .LPBX2");
  __gcov_info_to_gcda (my_info, filename, dump, allocate, &counter);
  assert (counter > 8);
  assert (check_zero);
  assert (check_after_filename);
  return 0;
}
