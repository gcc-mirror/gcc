/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

void bad_realloc(char *s, int n)
{
  char *p = __builtin_realloc(s, n);
} /* { dg-warning "leak" } */
