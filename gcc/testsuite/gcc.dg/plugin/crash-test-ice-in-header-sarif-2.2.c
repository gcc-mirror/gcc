/* Test of an ICE triggered within a header file with SARIF 2.2  */

/* { dg-do compile } */
/* { dg-options "-fdiagnostics-set-output=sarif:version=2.2-prerelease" } */
/* { dg-additional-options "-fno-report-bug" } */

#include "crash-test-ice-in-header.h"  /* { dg-ice "" } */
/* { dg-regexp "during GIMPLE pass: crash_test" } */

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file "2.2" } }

   { dg-final { run-sarif-pytest crash-test-ice-in-header-sarif-2.2.c "crash-test-ice-in-header-sarif-2_2.py" } } */
