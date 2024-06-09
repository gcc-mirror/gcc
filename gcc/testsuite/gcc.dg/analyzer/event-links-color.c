/* Verify colorization of event links (using ASCII).
   C only: we don't care about any C/C++ differences between source
   locations here.  */

/* { dg-additional-options "-fdiagnostics-path-format=inline-events" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers" } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */
/* { dg-additional-options "-fdiagnostics-show-event-links" } */
/* { dg-additional-options "-fdiagnostics-color=always" } */
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
      __builtin_free (p);
      break;
    case 43:
      break;
    }
}

/* { dg-begin-multiline-output "" }
   NN |       [01;35m[K__builtin_free (p)[m[K;
      |       [01;35m[K^~~~~~~~~~~~~~~~~~[m[K
  '[01m[Ktest[m[K': events 1-6
   NN |   if [01;36m[K([m[Kflag_a)
      |      [01;36m[K^[m[K
      |      [01;36m[K|[m[K
      |      [01;36m[K(1)[m[K following '[01m[Ktrue[m[K' branch (when '[01m[Kflag_a != 0[m[K')...[01;36m[K ->-+[m[K
      |                                                             [01;36m[K|[m[K
      |                                                             [01;36m[K|[m[K
      |[01;36m[K+[m[K[01;36m[K------------------------------------------------------------+[m[K
   NN |[01;36m[K|[m[K    [01;36m[K__builtin_free (p)[m[K;
      |[01;36m[K|[m[K    [01;36m[K~~~~~~~~~~~~~~~~~~[m[K
      |[01;36m[K|[m[K    [01;36m[K|[m[K
      |[01;36m[K+[m[K[01;36m[K--->[m[K[01;36m[K(2)[m[K ...to here
      |     [01;36m[K(3)[m[K first '[01m[Kfree[m[K' here
   NN |   [01;36m[Kswitch[m[K (val)
      |   [01;36m[K~~~~~~[m[K
      |   [01;36m[K|[m[K
      |   [01;36m[K(4)[m[K following '[01m[Kcase 42:[m[K' branch...[01;36m[K ->-+[m[K
      |                                         [01;36m[K|[m[K
......
      |                                         [01;36m[K|[m[K
      |[01;36m[K+[m[K[01;36m[K----------------------------------------+[m[K
   NN |[01;36m[K|[m[K    [01;36m[Kcase[m[K 42:
      |[01;36m[K|[m[K    [01;36m[K~~~~[m[K
      |[01;36m[K|[m[K    [01;36m[K|[m[K
      |[01;36m[K+[m[K[01;36m[K--->[m[K[01;36m[K(5)[m[K ...to here
   NN |       [01;36m[K__builtin_free (p)[m[K;
      |       [01;36m[K~~~~~~~~~~~~~~~~~~[m[K
      |       [01;36m[K|[m[K
      |       [01;36m[K(6)[m[K second '[01m[Kfree[m[K' here; first '[01m[Kfree[m[K' was at [01;36m[K(3)[m[K
    { dg-end-multiline-output "" } */

/* DejaGnu won't recognize the warning due to the colorization codes, 
   so skip it.  */
/* { dg-prune-output ".*" } */
