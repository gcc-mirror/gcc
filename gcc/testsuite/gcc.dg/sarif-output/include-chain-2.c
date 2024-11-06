/* { dg-require-effective-target analyzer } */
/* { dg-options "-fanalyzer -fdiagnostics-format=sarif-file" } */
/* { dg-do compile } */

/* Verify that SARIF output can capture chains of include files in
   diagnostic paths within result locations.

   Generate an analyzer warning with a path, using a chain of header files
   both for the warning and for the events within its esxecution path.
   In textual form, we'd expect something like:

In file included from PATH/include-chain-2.c:28:
PATH/include-chain-2.h: In function 'test':
PATH/include-chain-2.h:6:3: warning: double-'free' of 'ptr' [CWE-415] [-Wanalyzer-double-free]
    6 |   __builtin_free (ptr);
      |   ^~~~~~~~~~~~~~~~~~~~
  'test': events 1-2
    5 |   __builtin_free (ptr);
      |   ^~~~~~~~~~~~~~~~~~~~
      |   |
      |   (1) first 'free' here
    6 |   __builtin_free (ptr);
      |   ~~~~~~~~~~~~~~~~~~~~
      |   |
      |   (2) second 'free' here; first 'free' was at (1)
*/

#include "include-chain-2.h"

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest include-chain-2.c "include-chain-2.py" } } */
