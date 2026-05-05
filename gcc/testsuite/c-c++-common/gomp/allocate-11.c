/* This warning is effectively bogus, it appears to only trigger after UB(?)
   triggers an optimization that removes the case labels.  We don't actually
   want to check for this behavior.  If anything we might want to ensure it
   doesn't trigger when jumping over a variable found in an allocate directive,
   as we are supposed to already diagnose an error for that case.  */
/* { dg-additional-options -Wno-switch-unreachable } */

void bar();
void use (int*);

void
f (int i)
{
  switch (i)  /* { dg-note "switch starts here" "" { xfail c++ } } */
    {
      int j;  /* { dg-note "'j' declared here" "" { xfail c++ } } */
      #pragma omp allocate(j)
    case 42:  /* { dg-error "switch jumps over OpenMP 'allocate' allocation" "" { xfail c++ } } */
      bar ();
      break;
    case 51:  /* { dg-error "switch jumps over OpenMP 'allocate' allocation" "" { xfail c++ } } */
      use (&j);
      break;
    }
}

int
h (int i2)
{
  if (i2 == 5)
    goto label; /* { dg-error "jump skips OpenMP 'allocate' allocation" "" { xfail c++ } } */
    /* { dg-note "from here" "" { target c++ } .-1 } */
  return 5;

  int k2;  /* { dg-note "'k2' declared here" "" { xfail c++ } } */
  int j2 = 4;  /* { dg-note "'j2' declared here" "" { xfail c++ } } */
  /* { dg-note "crosses initialization of 'int j2'" "" { target c++ } .-1 } */
  #pragma omp allocate(k2, j2)
label:  /* { dg-note "label 'label' defined here" "" { xfail c++ } } */
// It might make sense to make this bogus, as semantically it's assigning to the pointed at value.
/* { dg-error "jump to label 'label'" "" { target c++ } .-2 } */
  k2 = 4;
  return j2 + k2;
}
