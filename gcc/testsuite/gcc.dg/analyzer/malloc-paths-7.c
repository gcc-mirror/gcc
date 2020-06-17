#include <stdlib.h>

extern int foo (void);
extern int bar (void);

void test (void)
{
  void *p = malloc (1024); /* { dg-message "\\(1\\) allocated here" } */ 
  void *q = malloc (1024);

  foo ();
  if (!q) /* { dg-message "\\(2\\) following 'true' branch \\(when 'q' is NULL\\)\\.\\.\\." } */ 
    {
      free (q); /* { dg-message "\\(3\\) \\.\\.\\.to here" } */ 
      return; /* { dg-warning "leak of 'p'" "warning" } */ 
      /* { dg-message "\\(4\\) 'p' leaks here; was allocated at \\(1\\)" "event" { target *-*-* } .-1 } */
    }
  bar ();
  free (q);
  free (p);
}
