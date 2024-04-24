// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

#[lang = "sized"]
pub trait Sized {}

fn immutable_borrow_while_immutable_borrowed() {
    let x = 0;
    let y = &x;
    let z = &x;
    let w = y;
}


fn immutable_borrow_while_mutable_borrowed() {
    // { dg-error "Found loan errors in function immutable_borrow_while_mutable_borrowed" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = &mut x;
    let z = &x; //~ ERROR
    let w = y;
}

fn mutable_borrow_while_immutable_borrowed() {
    // { dg-error "Found loan errors in function mutable_borrow_while_immutable_borrowed" "" { target *-*-* } .-1 }
    let x = 0;
    let y = &x;
    let z = &mut x; //~ ERROR
    let w = y;
}

fn mutable_borrow_while_mutable_borrowed() {
    // { dg-error "Found loan errors in function mutable_borrow_while_mutable_borrowed" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = &mut x;
    let z = &mut x; //~ ERROR
    let w = y;
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
}

fn read_while_mutable_borrowed() {
    // { dg-error "Found loan errors in function read_while_mutable_borrowed" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = &mut x;
    let z = x; //~ ERROR
    let w = y;
}

fn write_while_borrowed() {
    // { dg-error "Found loan errors in function write_while_borrowed" "" { target *-*-* } .-1 }
    let mut x = 0;
    let y = &x;
    x = 1; //~ ERROR
    let z = y;
}

fn write_while_immutable_borrowed() {
    // { dg-error "Found loan errors in function write_while_immutable_borrowed" "" { target *-*-* } .-1 }
    let x = 0;
    let y = &x;
    x = 1; //~ ERROR
    let z = y;
}