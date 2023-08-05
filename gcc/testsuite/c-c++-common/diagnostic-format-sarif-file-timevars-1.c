/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file -ftime-report" } */

#warning message

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* We expect various properties.
   The indentation here reflects the expected hierarchy, though these tests
   don't check for that, merely the string fragments we expect.

   { dg-final { scan-sarif-file {"invocations": } } }
     { dg-final { scan-sarif-file {"properties": } } }
       { dg-final { scan-sarif-file {"gcc/timeReport": } } }
         { dg-final { scan-sarif-file {"timevars": } } }
           { dg-final { scan-sarif-file {"name": "TOTAL",} } }
*/
