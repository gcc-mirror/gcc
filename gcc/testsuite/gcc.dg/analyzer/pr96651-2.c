#include "analyzer-decls.h"

extern void unknown_fn (void *, void *);

static int a;
static int b = 42;
int c;
int d = 17;
struct { int x; int y; char rgb[3]; } s = {5, 10, {0x80, 0x40, 0x20}};
void *e = &d;

extern struct _IO_FILE *stderr;

/* If we're not on a direct path from "main", we know nothing about
   the values of globals.  */

void test (void)
{
  __analyzer_eval (a == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (b == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (c == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (d == 17); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (s.rgb[2] == 0x20); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (e == &d); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (stderr == 0); /* { dg-warning "UNKNOWN" } */
}

static void __attribute__((noinline))
__analyzer_called_from_main (void)
{
  /* When accessed from main, the vars still have their initializer values.  */
  __analyzer_eval (a == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (b == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (c == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (d == 17); /* { dg-warning "TRUE" } */
  __analyzer_eval (s.rgb[2] == 0x20); /* { dg-warning "TRUE" } */
  __analyzer_eval (e == &d); /* { dg-warning "TRUE" } */
  /* ...apart from those defined in a different TU (or that were inited
     before "main").  */
  __analyzer_eval (stderr == 0); /* { dg-warning "UNKNOWN" } */
}

int main (void)
{
  /* When accessed from main, the vars still have their initializer values.  */
  __analyzer_eval (a == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (b == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (c == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (d == 17); /* { dg-warning "TRUE" } */
  __analyzer_eval (s.rgb[2] == 0x20); /* { dg-warning "TRUE" } */
  __analyzer_eval (e == &d); /* { dg-warning "TRUE" } */
  /* ...apart from those defined in a different TU (or that were inited
     before "main").  */
  __analyzer_eval (stderr == 0); /* { dg-warning "UNKNOWN" } */

  __analyzer_called_from_main ();

  unknown_fn (&a, &c);

  /* "a" escaped above and so could have been written to.  */
  __analyzer_eval (a == 0); /* { dg-warning "UNKNOWN" } */
  /* "b" doesn't escape and is static, and so must still have its
     initial value.  */
  __analyzer_eval (b == 42); /* { dg-warning "TRUE" } */
  /* The other globals are non-static and so have implicitly escaped,
     and so could have been written to.  */
  __analyzer_eval (c == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (d == 17); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (s.rgb[2] == 0x20); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (e == &d); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (stderr == 0); /* { dg-warning "UNKNOWN" } */
}
