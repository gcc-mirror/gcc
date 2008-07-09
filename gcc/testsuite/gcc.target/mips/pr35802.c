/* { dg-mips-options "-O2 -march=74kc -mgp32" } */
__thread int x __attribute__((tls_model("initial-exec")));
__thread int y __attribute__((tls_model("initial-exec")));

int bar (void);

NOMIPS16 void
foo (int n)
{
  if (n > 5)
    {
      y = 0;
      do
	x += bar ();
      while (n--);
    }
}
