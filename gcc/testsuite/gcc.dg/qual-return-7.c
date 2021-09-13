/* Same as qual-return-3.c but with nested functions.
   { dg-do compile }
   { dg-options "-std=gnu99" } */

void test_local (void)
{
  auto int foo ();                /* { dg-message "note: previous declaration" "different qualifiers" } */

  const int foo () { return 0; }  /* { dg-error "conflicting types" "different qualifiers" } */

  auto void bar (void);
  volatile void bar () { }        /* { dg-warning "qualified|volatile" "different qualifiers" } */

  auto volatile void baz (void);
  void baz () { }                 /* { dg-warning "not compatible" "different qualifiers" } */
}

/* { dg-prune-output "nested function 'foo' declared but never defined" } */
