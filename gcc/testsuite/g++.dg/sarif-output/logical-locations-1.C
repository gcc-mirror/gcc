/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Verify that we can capture the chain of parents of a
   logical location (PR 116176).  */

namespace ns {
  class foo
  {
    void bar ()
    {
      return 0;
    }
  };
}

/* We expect a failing compile due to the error, but the use of
   -fdiagnostics-format=sarif-file means there should be no output to stderr.
   DejaGnu injects this message; ignore it:
   { dg-prune-output "exit status is 1" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest logical-locations-1.C "logical-locations-1.py" } } */
