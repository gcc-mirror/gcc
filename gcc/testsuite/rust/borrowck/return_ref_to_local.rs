// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

pub fn return_ref_to_local() -> &'static i32 {
    let x = 0;
    &x //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    /*
     { dg-begin-multiline-output "" }
   NN |     &x //~ ERROR
      |     ^
      |     |
      |     borrow occurs here
      |     borrowed value used here
     { dg-end-multiline-output "" }
    */
}
