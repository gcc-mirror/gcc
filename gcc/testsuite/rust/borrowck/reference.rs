// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }


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
    // { dg-error "Found loan errors in function immutable_borrow_while_mutable_borrowed_struct" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = ReferenceMut::new(&mut x);
    let z = &x; //~ ERROR
    let w = y;
}

fn mutable_borrow_while_immutable_borrowed_struct() {
    // { dg-error "Found loan errors in function mutable_borrow_while_immutable_borrowed_struct" "" { target *-*-* } .-1 }
    let x = 0;
    let y = Reference::new(&x);
    let z = &mut x; //~ ERROR
    let w = y;
}

fn mutable_borrow_while_mutable_borrowed_struct() {
    // { dg-error "Found loan errors in function mutable_borrow_while_mutable_borrowed_struct" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = ReferenceMut::new(&mut x);
    let z = &mut x; //~ ERROR
    let w = y;
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
    let x = 0;
    let y = Reference::new(&x);
    let z = &mut *y.value; //~ ERROR
}

fn read_while_mutable_borrowed_struct() {
    // { dg-error "Found loan errors in function read_while_mutable_borrowed_struct" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = ReferenceMut::new(&mut x);
    let z = x; //~ ERROR
    let w = y;
}

fn write_while_borrowed_struct() {
    // { dg-error "Found loan errors in function write_while_borrowed_struct" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = Reference::new(&x);
    x = 1; //~ ERROR
    let z = y;
}

fn write_while_immutable_borrowed_struct() {
    // { dg-error "Found loan errors in function write_while_immutable_borrowed_struct" "" { target *-*-* } .-1 }
    let x = 0;
    let y = Reference::new(&x);
    x = 1; //~ ERROR
    let z = y;
}