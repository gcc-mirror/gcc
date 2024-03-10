/* TODO: enable for C++ once implemented. */
/* { dg-do compile { target c } } */

void bar();
void use (int*);

void
f (int i)
{
  switch (i)  /* { dg-note "switch starts here" } */
    {
      int j;  /* { dg-note "'j' declared here" } */
      #pragma omp allocate(j)
    case 42:  /* { dg-error "switch jumps over OpenMP 'allocate' allocation" } */
      bar ();
      /* { dg-warning "statement will never be executed \\\[-Wswitch-unreachable\\\]" "" { target *-*-* } .-1 } */
      break;
    case 51:  /* { dg-error "switch jumps over OpenMP 'allocate' allocation" } */
      use (&j);
      break;
    }
}

int
h (int i2)
{
  if (i2 == 5)
    goto label; /* { dg-error "jump skips OpenMP 'allocate' allocation" } */
  return 5;

  int k2;  /* { dg-note "'k2' declared here" } */
  int j2 = 4;  /* { dg-note "'j2' declared here" } */
  #pragma omp allocate(k2, j2)
label:  /* { dg-note "label 'label' defined here" } */
  k2 = 4;
  return j2 + k2;
}
