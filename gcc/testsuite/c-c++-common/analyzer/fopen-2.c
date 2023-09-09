typedef struct FILE FILE;
FILE *fopen (const char *pathname, const char *mode);
#include "../../gcc.dg/analyzer/analyzer-decls.h"

FILE *
test_passthrough (const char *pathname, const char *mode)
{
  return fopen (pathname, mode);
}

FILE *
test_null_pathname (const char *pathname, const char *mode)
{
  return fopen (NULL, mode);
}

FILE *
test_null_mode (const char *pathname)
{
  return fopen (pathname, NULL);
}

FILE *
test_simple_r (void)
{
  return fopen ("foo.txt", "r");
}

FILE *
test_swapped_args (void)
{
  return fopen ("r", "foo.txt"); /* TODO: would be nice to detect this.  */
}

FILE *
test_uninitialized_pathname (const char *mode)
{
  char buf[10];
  return fopen (buf, mode); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */  
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of 'fopen'..." "event" { target c } .-1 } */
  /* { dg-message "while looking for null terminator for argument 1 \\('& buf'\\) of 'FILE\\* fopen\\(const char\\*, const char\\*\\)'..." "event" { target c++ } .-2 } */
}

FILE *
test_uninitialized_mode (const char *filename)
{
  char buf[10];
  return fopen (filename, buf); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */  
  /* { dg-message "while looking for null terminator for argument 2 \\('&buf'\\) of 'fopen'..." "event" { target c } .-1 } */
  /* { dg-message "while looking for null terminator for argument 2 \\('& buf'\\) of 'FILE\\* fopen\\(const char\\*, const char\\*\\)'..." "event" { target c++ } .-2 } */
}

