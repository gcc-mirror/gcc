/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

extern void acquire_lock_a(void);
extern void acquire_lock_b(void);

void foo ()
{
  acquire_lock_a ();
  acquire_lock_b ();
}

void bar ()
{
  acquire_lock_b ();
  acquire_lock_a ();
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest diagnostic-test-paths-multithreaded-sarif.c "diagnostic-test-paths-multithreaded-sarif.py" } } */
