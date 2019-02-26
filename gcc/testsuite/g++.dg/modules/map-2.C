// { dg-additional-options "-fmodules-ts -fmodule-mapper=$srcdir/g++.dg/modules/map-2.map?@" }

#include <stdio.h>
#include <math.h>
// { dg-regexp "In module imported at \[^\n]*map-2.C:4:1:\n<math.h>: error: failed to read module 'math.h.bmi': No such file or directory\n" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
