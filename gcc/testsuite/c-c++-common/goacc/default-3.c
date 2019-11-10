/* OpenACC default (none) clause.  */

void f1 ()
{
  int f1_a = 2;
  float f1_b[2];

#pragma acc kernels default (none) /* { dg-note "enclosing OpenACC 'kernels' construct with 'default\\\(none\\\)' clause" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc parallel default (none) /* { dg-note "enclosing OpenACC 'parallel' construct with 'default\\\(none\\\)' clause" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc serial default (none) /* { dg-note "enclosing OpenACC 'serial' construct with 'default\\\(none\\\)' clause" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }

#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc kernels /* { dg-note "enclosing OpenACC 'kernels' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc parallel /* { dg-note "enclosing OpenACC 'parallel' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc serial /* { dg-note "enclosing OpenACC 'serial' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }

#pragma acc data default (none)
#pragma acc kernels default (none) /* { dg-note "enclosing OpenACC 'kernels' construct with 'default\\\(none\\\)' clause" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc data default (none)
#pragma acc parallel default (none) /* { dg-note "enclosing OpenACC 'parallel' construct with 'default\\\(none\\\)' clause" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc data default (none)
#pragma acc serial default (none) /* { dg-note "enclosing OpenACC 'serial' construct with 'default\\\(none\\\)' clause" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }

#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc data
#pragma acc data
#pragma acc kernels /* { dg-note "enclosing OpenACC 'kernels' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc data
#pragma acc data
#pragma acc parallel /* { dg-note "enclosing OpenACC 'parallel' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc data
#pragma acc data
#pragma acc serial /* { dg-note "enclosing OpenACC 'serial' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }

#pragma acc data
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc data
#pragma acc kernels /* { dg-note "enclosing OpenACC 'kernels' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc data
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc data
#pragma acc parallel /* { dg-note "enclosing OpenACC 'parallel' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc data
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc data
#pragma acc serial /* { dg-note "enclosing OpenACC 'serial' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }

#pragma acc data
#pragma acc data
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc kernels /* { dg-note "enclosing OpenACC 'kernels' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc data
#pragma acc data
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc parallel /* { dg-note "enclosing OpenACC 'parallel' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc data
#pragma acc data
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc serial /* { dg-note "enclosing OpenACC 'serial' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }

#pragma acc data
#pragma acc data default (none)
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc kernels /* { dg-note "enclosing OpenACC 'kernels' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc data
#pragma acc data default (none)
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc parallel /* { dg-note "enclosing OpenACC 'parallel' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
#pragma acc data
#pragma acc data default (none)
#pragma acc data default (none) /* { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" } */
#pragma acc serial /* { dg-note "enclosing OpenACC 'serial' construct and" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" } */
  }
}
