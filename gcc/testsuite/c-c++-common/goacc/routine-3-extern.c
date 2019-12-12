/* Test invalid calls to routines.  */
/* Variant of 'routine-3.c', moving the callees 'extern'.  */

#pragma acc routine gang
extern int extern_gang (); /* { dg-message "declared here" "3" } */

#pragma acc routine worker
extern int extern_worker (); /* { dg-message "declared here" "2" } */

#pragma acc routine vector
extern int extern_vector (); /* { dg-message "declared here" } */

#pragma acc routine seq
extern int extern_seq ();

int
main ()
{
  int red = 0;
#pragma acc parallel copy (red)
  {
    /* Independent/seq loop tests.  */
#pragma acc loop reduction (+:red) // { dg-warning "insufficient partitioning" }
    for (int i = 0; i < 10; i++)
      red += extern_gang ();

#pragma acc loop reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_worker ();

#pragma acc loop reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_vector ();

    /* Gang routine tests.  */
#pragma acc loop gang reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += extern_gang (); // { dg-error "routine call uses same" }

#pragma acc loop worker reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += extern_gang (); // { dg-error "routine call uses same" }

#pragma acc loop vector reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += extern_gang (); // { dg-error "routine call uses same" }

    /* Worker routine tests.  */
#pragma acc loop gang reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_worker ();

#pragma acc loop worker reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += extern_worker (); // { dg-error "routine call uses same" }

#pragma acc loop vector reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += extern_worker (); // { dg-error "routine call uses same" }

    /* Vector routine tests.  */
#pragma acc loop gang reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_vector ();

#pragma acc loop worker reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_vector ();

#pragma acc loop vector reduction (+:red)  /* { dg-message "containing loop" } */
    for (int i = 0; i < 10; i++)
      red += extern_vector (); // { dg-error "routine call uses same" }

    /* Seq routine tests.  */
#pragma acc loop gang reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_seq ();

#pragma acc loop worker reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_seq ();

#pragma acc loop vector reduction (+:red)
    for (int i = 0; i < 10; i++)
      red += extern_seq ();
  }

  return 0;
}
