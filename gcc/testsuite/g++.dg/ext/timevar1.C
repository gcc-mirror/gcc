// PR c++/52248
// { dg-options "-ftime-report" }
// { dg-allow-blank-lines-in-output 1 }
// { dg-prune-output "Time variable" }
// { dg-prune-output "k" }
// { dg-prune-output " 0 " }
// { dg-prune-output "checks" }
// { dg-prune-output "\[0-9\]+%" }

void
foo ()
{
  goto lab;			// { dg-error "not defined" }
}
