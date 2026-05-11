/* Smoketest for the "analyzer-graphs=yes" option to the sarif
   diagnostic sink.  */

/* { dg-additional-options "-fdiagnostics-add-output=sarif:analyzer-graphs=yes" } */

void double_free (int flag, void *p)
{
  if (flag)
    __builtin_free (p);
  __builtin_free (p); /* { dg-warning "double-'free' of 'p'" } */
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest sarif-analyzer-graphs.c "sarif-analyzer-graphs.py" } } */
