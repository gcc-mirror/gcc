/* C only: C++ FE fpermissive already emits an error for initializer
   string too long.
   C++ compatible parts have been moved into c-c++-common/analyzer/fopen-2.c  */

typedef struct FILE FILE;
FILE *fopen (const char *pathname, const char *mode);

FILE *
test_unterminated_pathname (const char *mode)
{
  char buf[3] = "abc";
  return fopen (buf, mode); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of 'fopen'..." "event" { target *-*-* } .-1 } */
}

FILE *
test_unterminated_mode (const char *filename)
{
  char buf[3] = "abc";
  return fopen (filename, buf);  /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&buf'\\) of 'fopen'..." "event" { target *-*-* } .-1 } */
}
