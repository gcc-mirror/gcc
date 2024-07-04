/* Verify that -fno-diagnostics-show-event-links works.
   C only: we don't care about any C/C++ differences between source
   locations here.  */

/* { dg-additional-options "-fdiagnostics-path-format=inline-events" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers" } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */
/* { dg-additional-options "-fno-diagnostics-show-event-links" } */
/* { dg-enable-nn-line-numbers "" } */

void test (int flag_a, int val, void *p)
{
  if (flag_a)
    __builtin_free (p);
  switch (val)
    {
    default:
      break;
    case 41:
      break;
    case 42:
      __builtin_free (p); /* { dg-warning "double-'free' of 'p'" } */
      break;
    case 43:
      break;
    }
}

/* { dg-begin-multiline-output "" }
   NN |       __builtin_free (p);
      |       ^~~~~~~~~~~~~~~~~~
  'test': events 1-6
   NN |   if (flag_a)
      |      ^
      |      |
      |      (1) following 'true' branch (when 'flag_a != 0')...
   NN |     __builtin_free (p);
      |     ~~~~~~~~~~~~~~~~~~
      |     |
      |     (2) ...to here
      |     (3) first 'free' here
   NN |   switch (val)
      |   ~~~~~~
      |   |
      |   (4) following 'case 42:' branch...
......
   NN |     case 42:
      |     ~~~~
      |     |
      |     (5) ...to here
   NN |       __builtin_free (p);
      |       ~~~~~~~~~~~~~~~~~~
      |       |
      |       (6) second 'free' here; first 'free' was at (3)
    { dg-end-multiline-output "" } */
