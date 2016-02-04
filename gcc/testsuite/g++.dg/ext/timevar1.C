// PR c++/52248
// { dg-options "-ftime-report" }
// { dg-allow-blank-lines-in-output 1 }
// { dg-prune-output "wall" }
// { dg-prune-output "times" }
// { dg-prune-output "TOTAL" }
// { dg-prune-output "checks" }

void
foo ()
{
  goto lab;			// { dg-error "not defined" }
}
