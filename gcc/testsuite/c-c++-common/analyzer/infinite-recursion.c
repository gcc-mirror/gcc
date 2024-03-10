extern void marker_A(void);
extern void marker_B(void);
extern void marker_C(void);
extern void marker_D(void);

void test(int flag)
{
  marker_A();

  if (flag) {
    marker_B();

    /* Recurse, infinitely, as it happens: */
    test(flag); /* { dg-warning "infinite recursion" } */

    marker_C();
  }

  marker_D();
}

/* A cycle of 4 mutually-recursive functions (but only for certain inputs).  */

extern void mutual_test_1 (int flag);
extern void mutual_test_2 (int flag);
extern void mutual_test_3 (int flag);
extern void mutual_test_4 (int flag);

void mutual_test_1 (int flag)
{
  marker_A ();
  if (flag)
    mutual_test_2 (flag); /* { dg-warning "infinite recursion" } */
}

void mutual_test_2 (int flag)
{
  marker_B ();
  if (flag)
    mutual_test_3 (flag); /* { dg-warning "infinite recursion" } */
}

void mutual_test_3 (int flag)
{
  marker_C ();
  if (flag)
    mutual_test_4 (flag); /* { dg-warning "infinite recursion" } */
}

void mutual_test_4 (int flag)
{
  marker_D ();
  if (flag)
    mutual_test_1 (flag); /* { dg-warning "infinite recursion" } */
}
