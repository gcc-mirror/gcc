/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif" } */

#include "this-does-not-exist.h"

/* { dg-prune-output "fatal error:" }
   { dg-prune-output "compilation terminated" }
   { dg-final { verify-sarif-file } }
   { dg-final { run-sarif-pytest fatal-error.c "fatal-error-sarif.py" } } */
