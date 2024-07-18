// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

#[lang = "sized"]
pub trait Sized {}

struct Reference<'a> {
    value: &'a i32,
}

impl<'a> Reference<'a> {
    fn new<'a>(value: &'a i32) -> Reference<'a> {
        Reference { value: value }
    }
}

struct ReferenceMut<'a> {
    value: &'a mut i32,
}

impl<'a> ReferenceMut<'a> {
    fn new<'a>(value: &'a mut i32) -> ReferenceMut<'a> {
        ReferenceMut { value: value }
    }
}

fn immutable_borrow_while_immutable_borrowed_struct() {
    let x = 0;
    let y = Reference::new(&x);
    let z = &x;
    let w = y;
}

fn immutable_borrow_while_mutable_borrowed_struct() {
    let mut x = 0;
    let y = ReferenceMut::new(&mut x);
    let z = &x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = ReferenceMut::new(&mut x);
      |                               ~
      |                               |
      |                               borrow occurs here
   NN |     let z = &x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
     */
}

fn mutable_borrow_while_immutable_borrowed_struct() {
    let x = 0;
    let y = Reference::new(&x);
    let z = &mut x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = Reference::new(&x);
      |                            ~
      |                            |
      |                            borrow occurs here
   NN |     let z = &mut x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
     */
}

fn mutable_borrow_while_mutable_borrowed_struct() {
    let mut x = 0;
    let y = ReferenceMut::new(&mut x);
    let z = &mut x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = ReferenceMut::new(&mut x);
      |                               ~
      |                               |
      |                               borrow occurs here
   NN |     let z = &mut x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
     */
}

fn immutable_reborrow_while_immutable_borrowed_struct() {
    let x = 0;
    let y = Reference::new(&x);
    let z = &*y.value;
}

fn immutable_reborrow_while_mutable_borrowed_struct() {
    let mut x = 0;
    let y = Reference::new(&mut x);
    let z = &*y.value;
}

fn mutable_reborrow_while_immutable_borrowed_struct() {
    // { dg-error "Cannot reborrow immutable borrow as mutable" "" { target *-*-* } .-1 }
    /*
    { dg-begin-multiline-output "" }
   NN | fn mutable_reborrow_while_immutable_borrowed_struct() {
      | ^~
    { dg-end-multiline-output "" }
    */
    let x = 0;
    let y = Reference::new(&x);
    let z = &mut *y.value; //~ ERROR
}

fn read_while_mutable_borrowed_struct() {
    let mut x = 0;
    let y = ReferenceMut::new(&mut x);
    let z = x; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let w = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = ReferenceMut::new(&mut x);
      |                               ~
      |                               |
      |                               borrow occurs here
   NN |     let z = x; //~ ERROR
      |             ^
      |             |
      |             borrowed value used here
     { dg-end-multiline-output "" }
     */
}

fn write_while_borrowed_struct() {
    let mut x = 0;
    let y = Reference::new(&x);
    x = 1; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let z = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = Reference::new(&x);
      |                            ~
      |                            |
      |                            borrow occurs here
   NN |     x = 1; //~ ERROR
      |     ^
      |     |
      |     borrowed value used here
     { dg-end-multiline-output "" }
     */
}

fn write_while_immutable_borrowed_struct() {
    let x = 0;
    let y = Reference::new(&x);
    x = 1; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    let z = y;
    /*
     { dg-begin-multiline-output "" }
   NN |     let y = Reference::new(&x);
      |                            ~
      |                            |
      |                            borrow occurs here
   NN |     x = 1; //~ ERROR
      |     ^
      |     |
      |     borrowed value used here
     { dg-end-multiline-output "" }
     */
}
