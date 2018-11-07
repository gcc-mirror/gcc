// { dg-options "-Wformat -fdiagnostics-show-caret" }

void test_1 (void)
{
  __builtin_printf ("%s%s", 42, 43); // { dg-warning "argument 2 has type 'int'" }
  // { dg-warning "argument 3 has type 'int'" "" { target *-*-* } .-1 }
  /* { dg-begin-multiline-output "" }
   __builtin_printf ("%s%s", 42, 43);
                      ~^     ~~
                       |     |
                       char* int
                      %d
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   __builtin_printf ("%s%s", 42, 43);
                        ~^       ~~
                         |       |
                         char*   int
                        %d
     { dg-end-multiline-output "" } */
}

void test_2 (void)
{
  __builtin_printf ("before %s after", 6 * 7); // { dg-warning "argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   __builtin_printf ("before %s after", 6 * 7);
                             ~^         ~~~~~
                              |           |
                              char*       int
                             %d
     { dg-end-multiline-output "" } */
}
