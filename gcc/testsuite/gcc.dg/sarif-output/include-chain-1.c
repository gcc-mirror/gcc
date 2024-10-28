/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Verify that SARIF output can capture chains of include files in
   result locations.

   Generate two warning/note pairs, using a chain of header files.
   In textual form, we'd expect something like:

In file included from PATH/include-chain-1.h:5,
                 from PATH/include-chain-1.c:9:
PATH/include-chain-1-2.h:1:6: error: conflicting types for 'p'; have 'char'
    1 | char p;
      |      ^
In file included from PATH/include-chain-1.h:2:
PATH/include-chain-1-1.h:1:5: note: previous declaration of 'p' with type 'int'
    1 | int p;
      |     ^
PATH/include-chain-1-2.h:2:6: error: conflicting types for 'q'; have 'char'
    2 | char q;
      |      ^
PATH/include-chain-1-1.h:2:5: note: previous declaration of 'q' with type 'int'
    2 | int q;
      |     ^

    We should have two result objects (for each of 'p' and 'q'), each with
    a related location for its note, and additional related locations describing
    the include chains.  */

#include "include-chain-1.h"

/* We expect a failing compile due to the errors, but the use of 
   -fdiagnostics-format=sarif-file means there should be no output to stderr.
   DejaGnu injects this message; ignore it:
   { dg-prune-output "exit status is 1" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest include-chain-1.c "include-chain-1.py" } } */
