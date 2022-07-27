/* { dg-additional-options "-fdump-analyzer-untracked" } */

#include "analyzer-decls.h"

struct st
{
  const char *m_filename;
  int m_line;
};

typedef struct boxed_int { int value; } boxed_int;

extern void extern_fn (struct st *);
static void __attribute__((noinline)) internal_fn (struct st *) {}
extern int extern_get_int (void);
extern void extern_fn_char_ptr (const char *);

void test_0 (void)
{
  /* Not ever referenced; will get optimized away before
     analyzer ever sees it, so no message.  */
  static struct st s1 = { __FILE__, __LINE__ };
}

void test_1 (void)
{
  static struct st s1 = { __FILE__, __LINE__ }; /* { dg-warning "track 's1': no" } */
  extern_fn (&s1);
}

static struct st s2 = { __FILE__, __LINE__ }; /* { dg-warning "track 's2': yes" } */

void test_2 (void)
{
  extern_fn (&s2);
}

void test_3 (void)
{
  struct st s3 = { __FILE__, __LINE__ }; /* { dg-warning "track 's3': yes" } */
  extern_fn (&s3);
}

void test_3a (void)
{
  struct st s3a = { "foo.c", 42 }; /* { dg-warning "track 's3a': yes" } */
  __analyzer_eval (s3a.m_filename[0] == 'f'); /* { dg-warning "TRUE" } */
  __analyzer_eval (s3a.m_line == 42); /* { dg-warning "TRUE" } */
  extern_fn (&s3a);
  __analyzer_eval (s3a.m_filename[0] == 'f'); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (s3a.m_line == 42); /* { dg-warning "UNKNOWN" } */
}

extern void called_by_test_4 (int *);

int test_4 (void)
{
  int i; /* { dg-warning "track 'i': yes" } */
  called_by_test_4 (&i);
  return i;
}

void test_5 (int i)
{
  boxed_int bi5 = { i }; /* { dg-warning "track 'bi5': yes" } */
}

int test_6 (int i)
{
  static boxed_int bi6; /* { dg-warning "track 'bi6': yes" } */
  bi6.value = i;
  return bi6.value;
}

int test_7 (void)
{
  boxed_int bi7; /* { dg-warning "track 'bi7': yes" } */
  return bi7.value; /* { dg-warning "use of uninitialized value 'bi7.value'" "uninit" } */
}

void test_8 (void)
{
  static struct st s8 = { __FILE__, __LINE__ }; /* { dg-warning "track 's8': no" } */
  extern_fn (&s8);
  extern_fn (&s8);
}

void test_9 (void)
{
  static struct st s9 = { __FILE__, __LINE__ }; /* { dg-warning "track 's9': yes" } */
  internal_fn (&s9);
}

int test_10 (void)
{
  static struct st s10 = { __FILE__, __LINE__ }; /* { dg-warning "track 's10': yes" } */
  extern_fn (&s10);
  return s10.m_line;
}

int test_11 (void)
{
  static struct st s10 = { __FILE__, __LINE__ }; /* { dg-warning "track 's10': yes" } */
  s10.m_line = extern_get_int ();
  return 42;
}

int test_12 (void (*fnptr) (struct st *))
{
  static struct st s12 = { __FILE__, __LINE__ }; /* { dg-warning "track 's12': yes" } */
  fnptr (&s12);
}

void test_13 (void)
{
  extern_fn_char_ptr (__func__); /* { dg-warning "track '__func__': no" } */
}

char t14_global_unused[100]; /* { dg-warning "track 't14_global_unused': yes" } */
static char t14_static_unused[100]; /* { dg-warning "track 't14_static_unused': yes" } */
char t14_global_used[100]; /* { dg-warning "track 't14_global_used': yes" } */
static char t14_static_used[100]; /* { dg-warning "track 't14_static_used': yes" } */
void test_14 (void)
{
  extern_fn_char_ptr (t14_global_unused);
  extern_fn_char_ptr (t14_static_unused);
  extern_fn_char_ptr (t14_global_used);
  __analyzer_eval (t14_global_used[0] == '\0'); /* { dg-warning "UNKNOWN" } */
  extern_fn_char_ptr (t14_static_used);
  __analyzer_eval (t14_static_used[0] == '\0'); /* { dg-warning "UNKNOWN" } */
}
