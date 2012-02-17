// PR c++/52248
// { dg-options "-ftime-report" }
// { dg-prune-output "wall" }
// { dg-prune-output "times" }
// { dg-prune-output "TOTAL" }
// { dg-prune-output "checks" }

void
foo ()
{
  goto lab;			// { dg-error "not defined" }
}
