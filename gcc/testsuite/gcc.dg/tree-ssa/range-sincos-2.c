// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp -fno-thread-jumps" }

void use (double);
void link_error ();

void
foo (double x)
{
  double y;
  if (x >= 0.5 && x <= 1.3)
    {
      y = __builtin_sin (x);
      if (y < 0.45 || y > 0.97)
	link_error ();
      use (y);
    }
  if (x >= 0.5 && x < 1.75)
    {
      y = __builtin_sin (x);
      if (y < 0.45 || y > 1.05)
	link_error ();
      use (y);
    }
  if (x >= -1.57 && x <= 1.57)
    {
      y = __builtin_cos (x);
      if (y < 0.0007 || y > 1.05)
	link_error ();
      use (y);
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" { target { { *-*-linux* } && { glibc } } } } }
