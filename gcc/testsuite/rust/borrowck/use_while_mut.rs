// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

pub fn use_while_mut() {
    let mut x = 0;
    let y = &mut x;
    let z = x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
    { dg-begin-multiline-output "" }
   NN |     let y = &mut x;
      |             ~
      |             |
      |             borrow occurs here
   NN |     let z = x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
        { dg-end-multiline-output "" }
    */
}
