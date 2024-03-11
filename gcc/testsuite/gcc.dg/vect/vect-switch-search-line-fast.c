/* PR116126 -- once this works use this version in libcpp/lex.c.
   This also requires working value range propagation for s/end.  */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

const unsigned char *search_line_fast2 (const unsigned char *s,
					const unsigned char *end)
{
  while (s < end) {
    if (*s == '\n' || *s == '\r' || *s == '\\' || *s == '?')
      break;
    s++;
  }
  return s;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ilp32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { ! ilp32 } } } } */
