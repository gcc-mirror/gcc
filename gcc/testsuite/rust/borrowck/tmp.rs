// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

#[lang = "sized"]
pub trait Sized {}

fn immutable_borrow_while_immutable_borrowed() {
    let x = 0;
    let y = &x;
    let z = &x;
    let w = y;
}


fn immutable_borrow_while_mutable_borrowed() {
    let mut x = 0;
    let y = &mut x;
    let z = &x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = &mut x;
      |             ~
      |             |
      |             borrow occurs here
   NN |     let z = &x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
    */
}

fn mutable_borrow_while_immutable_borrowed() {
    let x = 0;
    let y = &x;
    let z = &mut x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = &x;
      |             ~
      |             |
      |             borrow occurs here
   NN |     let z = &mut x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
    */
}

fn mutable_borrow_while_mutable_borrowed() {
    let mut x = 0;
    let y = &mut x;
    let z = &mut x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = &mut x;
      |             ~
      |             |
      |             borrow occurs here
   NN |     let z = &mut x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
    */
}

fn immutable_reborrow_while_immutable_borrowed() {
    let x = 0;
    let y = &x;
    let z = &*y;
}

fn immutable_reborrow_while_mutable_borrowed() {
    let mut x = 0;
    let y = &mut x;
    let z = &*y;
}

fn mutable_reborrow_while_immutable_borrowed() {
    // { dg-error "Cannot reborrow immutable borrow as mutable" "" { target *-*-* } .-1 }
    let x = 0;
    let y = &x;
    let z = &mut *y; //~ ERROR
    /*
     { dg-begin-multiline-output "" }
   NN | fn mutable_reborrow_while_immutable_borrowed() {
      | ^~
     { dg-end-multiline-output "" }
    */
}

fn read_while_mutable_borrowed() {
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

fn write_while_borrowed() {
    let mut x = 0;
    let y = &x;
    x = 1; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let z = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = &x;
      |             ~
      |             |
      |             borrow occurs here
   NN |     x = 1; //~ ERROR
      |     ^
      |     |
      |     borrowed value used here
     { dg-end-multiline-output "" }
    */
}

fn write_while_immutable_borrowed() {
    let x = 0;
    let y = &x;
    x = 1; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let z = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = &x;
      |             ~
      |             |
      |             borrow occurs here
   NN |     x = 1; //~ ERROR
      |     ^
      |     |
      |     borrowed value used here
     { dg-end-multiline-output "" }
    */
}
