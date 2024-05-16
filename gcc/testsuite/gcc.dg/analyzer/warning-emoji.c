/* Verify that the final event in an analyzer path gets a "warning" emoji 
   when -fdiagnostics-text-art-charset=emoji (and
   -fdiagnostics-path-format=inline-events).  */

/* { dg-additional-options "-fdiagnostics-show-line-numbers" } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events" } */
/* { dg-additional-options "-fdiagnostics-text-art-charset=emoji" } */
/* { dg-enable-nn-line-numbers "" } */

void test (void *p)
{
  __builtin_free (p);
  __builtin_free (p); /* { dg-warning "double-'free'" } */
}

/* { dg-begin-multiline-output "" }
   NN |   __builtin_free (p);
      |   ^~~~~~~~~~~~~~~~~~
  'test': events 1-2
   NN |   __builtin_free (p);
      |   ^~~~~~~~~~~~~~~~~~
      |   |
      |   (1) first 'free' here
   NN |   __builtin_free (p);
      |   ~~~~~~~~~~~~~~~~~~
      |   |
      |   (2) ⚠️  second 'free' here; first 'free' was at (1)
   { dg-end-multiline-output "" } */
