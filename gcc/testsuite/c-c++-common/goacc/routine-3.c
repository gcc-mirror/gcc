/* Test invalid calls to routines.  */

#pragma acc routine gang
int
gang () /* { dg-message "declared here" 3 } */
{
  #pragma acc loop gang worker vector
  for (int i = 0; i < 10; i++)
    {
    }

  return 1;
}

#pragma acc routine worker
int
worker () /* { dg-message "declared here" 2 } */
{
  #pragma acc loop worker vector
  for (int i = 0; i < 10; i++)
    {
    }

  return 1;
}

#pragma acc routine vector
int
vector () /* { dg-message "declared here" } */
{
  #pragma acc loop vector
  for (int i = 0; i < 10; i++)
    {
    }

  return 1;
}

#pragma acc routine seq
int
seq ()
{
  return 1;
}

int
main ()
{
  int red = 0;
#pragma acc parallel copy (red)
  {
    /* Independent/seq loop tests.  */
#pragma acc loop reduction (+:red) // { dg-warning "insufficient partitioning" }
    for (int i = 0; i < 10; i++)
      red += gang ();

#pragma acc loop reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += worker ();

#pragma acc loop reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += vector ();

    /* Gang routine tests.  */
#pragma acc loop gang reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += gang (); // { dg-error "routine call uses same" }

#pragma acc loop worker reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += gang (); // { dg-error "routine call uses same" }

#pragma acc loop vector reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += gang (); // { dg-error "routine call uses same" }

    /* Worker routine tests.  */
#pragma acc loop gang reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += worker ();

#pragma acc loop worker reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += worker (); // { dg-error "routine call uses same" }

#pragma acc loop vector reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += worker (); // { dg-error "routine call uses same" }

    /* Vector routine tests.  */
#pragma acc loop gang reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += vector ();

#pragma acc loop worker reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += vector ();

#pragma acc loop vector reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += vector (); // { dg-error "routine call uses same" }

    /* Seq routine tests.  */
#pragma acc loop gang reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += seq ();

#pragma acc loop worker reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += seq ();

#pragma acc loop vector reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += seq ();
  }

  return 0;
}
