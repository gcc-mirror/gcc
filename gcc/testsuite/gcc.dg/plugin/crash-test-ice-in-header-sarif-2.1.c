/* Test of an ICE triggered within a header file with SARIF 2.1  */

/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */
/* { dg-additional-options "-fno-report-bug" } */

#include "crash-test-ice-in-header.h"  /* { dg-ice "" } */
/* { dg-regexp "during GIMPLE pass: crash_test" } */

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file "2.1" } }

   { dg-final { run-sarif-pytest crash-test-ice-in-header-sarif-2.1.c "crash-test-ice-in-header-sarif-2_1.py" } } */
