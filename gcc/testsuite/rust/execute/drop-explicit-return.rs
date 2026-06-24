// { dg-output "unit\r*\nmake_unit\r*\nunit_expr\r*\nmake_value\r*\nnonunit\r*\ninner\r*\nouter\r*\n" }
// { dg-additional-options "-w" }
#![feature(no_core)]
#![feature(lang_items)]
#![no_core]

extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "drop"]
pub trait Drop {
    fn drop(&mut self);
}

struct UnitDroppable;
struct UnitExprDroppable;
struct NonUnitDroppable;
struct OuterDroppable;
struct InnerDroppable;

impl Drop for UnitDroppable {
    fn drop(&mut self) {
        let msg = "unit\n\0" as *const str as *const i8;
        unsafe { printf(msg); }
    }
}
impl Drop for UnitExprDroppable {
    fn drop(&mut self) {
        let msg = "unit_expr\n\0" as *const str as *const i8;
        unsafe { printf(msg); }
    }
}

impl Drop for NonUnitDroppable {
    fn drop(&mut self) {
        let msg = "nonunit\n\0" as *const str as *const i8;
        unsafe { printf(msg); }
    }
}

impl Drop for OuterDroppable {
    fn drop(&mut self) {
        let msg = "outer\n\0" as *const str as *const i8;
        unsafe { printf(msg); }
    }
}

impl Drop for InnerDroppable {
    fn drop(&mut self) {
        let msg = "inner\n\0" as *const str as *const i8;
        unsafe { printf(msg); }
    }
}

fn make_unit () {
    let msg = "make_unit\n\0" as *const str as *const i8;
    unsafe { printf(msg); }
}

fn make_value () -> i32 {
    let msg = "make_value\n\0" as *const str as *const i8;
    unsafe { printf(msg); }
    42
}

fn unit_return () {
    let _x = UnitDroppable;
    return;
}

fn unit_return_expr () {
    let _x = UnitExprDroppable;
    return make_unit();
}

fn non_unit_return () -> i32 {
    let _x = NonUnitDroppable;
    return make_value();
}

fn nested_return() {
    let _outer = OuterDroppable;
    {
        let _inner = InnerDroppable;
        return;
    }
}

fn main() -> i32 {
    unit_return ();
    unit_return_expr ();

    let value = non_unit_return ();
    if value != 42 {
        return 1;
    }

    nested_return();

    0
}