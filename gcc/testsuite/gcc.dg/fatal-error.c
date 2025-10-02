/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif" } */
/* { dg-additional-options "-fdiagnostics-add-output=experimental-html:javascript=no" } */

#include "this-does-not-exist.h"

/* { dg-prune-output "fatal error:" }
   { dg-prune-output "compilation terminated" }
   { dg-final { verify-sarif-file } }
   { dg-final { run-sarif-pytest fatal-error.c "fatal-error-sarif.py" } }
   { dg-final { run-html-pytest fatal-error.c "fatal-error-html.py" } } */
