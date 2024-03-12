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

/* We expect various properties.
   The indentation here reflects the expected hierarchy, though these tests
   don't check for that, merely the string fragments we expect.

   { dg-final { scan-sarif-file {"version": "2.1.0"} } }
     { dg-final { scan-sarif-file {"text": "deadlock due to inconsistent lock acquisition order"} } }
     { dg-final { scan-sarif-file {"id": "Thread 1"} } }
       { dg-final { scan-sarif-file {"executionOrder": 1} } }
       { dg-final { scan-sarif-file {"executionOrder": 2} } }
       { dg-final { scan-sarif-file {"executionOrder": 5} } }
     { dg-final { scan-sarif-file {"id": "Thread 2"} } }
       { dg-final { scan-sarif-file {"executionOrder": 3} } }
       { dg-final { scan-sarif-file {"executionOrder": 4} } }
       { dg-final { scan-sarif-file {"executionOrder": 6} } }  */
