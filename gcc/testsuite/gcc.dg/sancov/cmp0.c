/* Basic test on number of inserted callbacks.  */
/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=trace-cmp -fdump-tree-optimized" } */

void
foo (char *a, short *b, int *c, long long *d, float *e, double *f)
{
  if (*a)
    *a += 1;
  if (*b)
    *b = *a;
  if (*c)
    *c += 1;
  if (*d)
    *d = *c;
  if (*e == *c)
    *e = *c;
  if (*f == *e)
    *f = *e;
  switch (*a)
    {
    case 2:
      *b += 2;
      break;
    case 3:
      *b += 3;
      break;
    case 4:
      *b += 4;
      break;
    case 5:
      *b += 5;
      break;
    case 6:
      *b += 6;
      break;
    case 7 ... 24:
      *b += 7;
      break;
    default:
      break;
    }
  switch (*d)
    {
    case 3:
      *d += 3;
    case -4:
      *d -= 4;
    case -5:
      *d -= 5;
    case -6:
      *d -= 6;
    case -7:
      *d -= 7;
    case -8:
      *d -= 8;
    case -9:
      *d -= 9;
    case -10:
      *d -= 10;
    }
}

void
bar (int *c)
{
  if (*c == 27)
    *c += 2;
  if (*c == 37)
    *c += 2;
}

int
baz (int *c, long long d, long long e)
{
  *c = (*c == 48) ? 12 : 24;
  return d == e;
}

/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp1 \\(0, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp2 \\(0, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp4 \\(0, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp8 \\(0, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp4 \\(27, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp4 \\(37, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp4 \\(48, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_cmp8 \\(" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_cmpf \\(" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_cmpd \\(" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_const_cmp" 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_cmp" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_switch \\(" 2 "optimized" } } */
