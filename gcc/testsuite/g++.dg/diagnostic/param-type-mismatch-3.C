// { dg-do compile { target c++14 } }
// { dg-options "-fdiagnostics-show-caret" }

void f2(int, char (*)(int), int) { } // { dg-line f2 }

void test_1 ()
{
  auto glambda = [](auto a) { return a; }; // { dg-line candidate }
  int (*fp)(int) = glambda;
  f2(1, glambda, 3); // { dg-error "invalid user-defined conversion" }
  /* { dg-begin-multiline-output "" }
   f2(1, glambda, 3);
         ^~~~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "candidate is: " "" { target *-*-* } candidate }
  /* { dg-begin-multiline-output "" }
   auto glambda = [](auto a) { return a; };
                  ^
     { dg-end-multiline-output "" } */
  // { dg-message "no known conversion from " "" { target *-*-* } candidate }
  // { dg-message "initializing argument 2 of " "" { target *-*-* } f2 }
  /* { dg-begin-multiline-output "" }
 void f2(int, char (*)(int), int) { }
              ^~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}
