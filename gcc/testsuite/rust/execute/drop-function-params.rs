// { dg-output "^l\r*\np\r*\nl\r*\np\r*\nl\r*\np2\r*\np1\r*\n$" }
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

struct ParamDroppable;
struct LocalDroppable;
struct FirstParamDroppable {
    value: i32,
}
struct SecondParamDroppable {
    value: i32,
}

impl Drop for ParamDroppable {
    fn drop(&mut self) {
        let msg = "p\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for LocalDroppable {
    fn drop(&mut self) {
        let msg = "l\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for FirstParamDroppable {
    fn drop(&mut self) {
        let msg = "p1\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for SecondParamDroppable {
    fn drop(&mut self) {
        let msg = "p2\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

fn named_param(_p: ParamDroppable) {
    let _l = LocalDroppable;
}

fn wildcard_param(_: ParamDroppable) {
    let _l = LocalDroppable;
}

fn multiple_params(_p1: FirstParamDroppable, _p2: SecondParamDroppable) {
    let _l = LocalDroppable;
}

fn main() -> i32 {
    named_param(ParamDroppable);
    wildcard_param(ParamDroppable);
    multiple_params(
        FirstParamDroppable { value: 1 },
        SecondParamDroppable { value: 2 },
    );
    0
}
