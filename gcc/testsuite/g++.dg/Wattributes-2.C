// Test to verify that attributes on distinct overloads of a function
//  with the same name are properly looked up and applied.
// { dg-do compile }
// { dg-options "-Wall" }

int
foo (int);

int __attribute__ ((noreturn))
foo (int, int);

int __attribute__ ((warn_unused_result))
foo (int, int, int);

int call_foo_1 ()
{
  foo (1);
}                       // { dg-warning "\\\[-Wreturn-type]" }

int call_foo_2 ()
{
  foo (1, 2);
}

int call_foo_3 ()
{
  foo (1, 2, 3);        // { dg-warning "\\\[-Wunused-result]" }
}                       // { dg-warning "\\\[-Wreturn-type]" }

int call_foo_4 ()
{
  // Make sure an error doesn't trigger bogus warnings or an ICE.
  foo (1, 2, 3, 4);     // { dg-error "no matching function" }
  return 0;
}
