// PR c++/52248
// { dg-options "-ftime-report" }
// { dg-allow-blank-lines-in-output 1 }
// { dg-prune-output "Time variable" }
// { dg-prune-output " kB" }
// { dg-prune-output "checks" }

void
foo ()
{
  goto lab;			// { dg-error "not defined" }
}
