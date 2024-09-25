// PR c++/115165
// { dg-additional-options "-fmodules-ts -ftime-report" }
// { dg-allow-blank-lines-in-output 1 }
// { dg-prune-output "Time variable" }
// { dg-prune-output "\[0-9\]+%" }
// { dg-prune-output "TOTAL" }
// { dg-prune-output "checks" }

import "timevar-1_a.H";
X x;
