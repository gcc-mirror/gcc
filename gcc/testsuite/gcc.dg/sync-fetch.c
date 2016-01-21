/* PR c/69405 - [6 Regression] ICE in c_tree_printer on an invalid
   __atomic_fetch_add */
/* Test to verify that the diagnostic doesn't cause an ICE when any
   of the arguments to __atomic_fetch_OP is undeclared.  */
/* { dg-do compile } */

void test_add_undeclared_first_arg (void)
{
  int a = 0;
  __atomic_fetch_add (&a, &b, 0);   /* { dg-error ".b. undeclared" } */
}

void test_sub_undeclared_first_arg (void)
{
  int a = 0;
  __atomic_fetch_sub (&a, &b, 0);      /* { dg-error ".b. undeclared" } */
}

void test_or_undeclared_first_arg (void)
{
  int a = 0;
  __atomic_fetch_or (&a, &b, 0);      /* { dg-error ".b. undeclared" } */
}

void test_and_undeclared_first_arg (void)
{
  int a = 0;
  __atomic_fetch_and (&a, &b, 0);      /* { dg-error ".b. undeclared" } */
}

void test_xor_undeclared_first_arg (void)
{
  int a = 0;
  __atomic_fetch_xor (&a, &b, 0);      /* { dg-error ".b. undeclared" } */
}

void test_nand_undeclared_first_arg (void)
{
  int a = 0;
  __atomic_fetch_nand (&a, &b, 0);      /* { dg-error ".b. undeclared" } */
}


void test_add_undeclared_second_arg (void)
{
  int b = 0;
  __atomic_fetch_add (&a, &b, 0);   /* { dg-error ".a. undeclared" } */
}

void test_sub_undeclared_second_arg (void)
{
  int b = 0;
  __atomic_fetch_sub (&a, &b, 0);      /* { dg-error ".a. undeclared" } */
}

void test_or_undeclared_second_arg (void)
{
  int b = 0;
  __atomic_fetch_or (&a, &b, 0);      /* { dg-error ".a. undeclared" } */
}

void test_and_undeclared_second_arg (void)
{
  int b = 0;
  __atomic_fetch_and (&a, &b, 0);      /* { dg-error ".a. undeclared" } */
}

void test_xor_undeclared_second_arg (void)
{
  int b = 0;
  __atomic_fetch_xor (&a, &b, 0);      /* { dg-error ".a. undeclared" } */
}

void test_nand_undeclared_second_arg (void)
{
  int b = 0;
  __atomic_fetch_nand (&a, &b, 0);      /* { dg-error ".a. undeclared" } */
}


void test_add_undeclared_third_arg (void)
{
  int a = 0, b = 0;
  __atomic_fetch_add (&a, &b, m);   /* { dg-error ".m. undeclared" } */
}

void test_sub_undeclared_third_arg (void)
{
  int a = 0, b = 0;
  __atomic_fetch_sub (&a, &b, m);      /* { dg-error ".m. undeclared" } */
}

void test_or_undeclared_third_arg (void)
{
  int a = 0, b = 0;
  __atomic_fetch_or (&a, &b, m);      /* { dg-error ".m. undeclared" } */
}

void test_and_undeclared_third_arg (void)
{
  int a = 0, b = 0;
  __atomic_fetch_and (&a, &b, m);      /* { dg-error ".m. undeclared" } */
}

void test_xor_undeclared_third_arg (void)
{
  int a = 0, b = 0;
  __atomic_fetch_xor (&a, &b, m);      /* { dg-error ".m. undeclared" } */
}

void test_nand_undeclared_third_arg (void)
{
  int a = 0, b = 0;
  __atomic_fetch_nand (&a, &b, m);      /* { dg-error ".m. undeclared" } */
}
