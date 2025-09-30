/* { dg-do run } */

/* #pragma GCC suppress_coverage should be a no-op and harmless when coverage is
   not enabled (--coverage, -fcondition-coverage, etc.).  */

int do_something (int x) {
    return x;
}

int main (int argc, char **argv)
{
#pragma GCC suppress_coverage begin
  int b = argc + 1;
#pragma GCC suppress_coverage end

#pragma GCC suppress_coverage begin
  int c;
#pragma GCC suppress_coverage end

  int a = argc;

  if (a)
    if (b)
      {
#pragma GCC suppress_coverage begin
      c = do_something (4);
#pragma GCC suppress_coverage end
      }
    else
      c = do_something (1024);

#pragma GCC suppress_coverage begin
  int d = a + c - 1;
#pragma GCC suppress_coverage end
}
