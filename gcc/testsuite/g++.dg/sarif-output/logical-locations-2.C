/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Verify that we correctly consolidate logical locations
   involving repeated diagnostics within a nested hierarchy
   (PR 116176).  */

namespace ns_outer {
  namespace ns_inner_1 {
    class klass_1
    {
      void member_fn_1 ()
      {
	return 0;
      }
      void member_fn_2 ()
      {
	return 0;
      }
    };
    class klass_2
    {
      void member_fn_1 ()
      {
	return 0;
      }
      void member_fn_2 ()
      {
	return 0;
      }
    };
  } // ns_inner_1
  namespace ns_inner_2 {
    class klass_1
    {
      void member_fn_1 ()
      {
	return 0;
      }
      void member_fn_2 ()
      {
	return 0;
      }
    };
    class klass_2
    {
      void member_fn_1 ()
      {
	return 0;
      }
      void member_fn_2 ()
      {
	return 0;
      }
    };
  } // ns_inner_2
} // ns_outer

/* We expect a failing compile due to the error, but the use of
   -fdiagnostics-format=sarif-file means there should be no output to stderr.
   DejaGnu injects this message; ignore it:
   { dg-prune-output "exit status is 1" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest logical-locations-2.C "logical-locations-2.py" } } */
