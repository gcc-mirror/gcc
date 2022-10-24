// { dg-do compile }
// { dg-options "-O2 -fno-guess-branch-probability -fsanitize=float-cast-overflow --param=max-jump-thread-duplication-stmts=240" }

float f;

void
foo (double d)
{
  (char) f;
  long l = __builtin_fabs (d);
  (char) f;
  (long) d;
}
