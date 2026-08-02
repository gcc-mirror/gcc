// { dg-output "^moved\r*\nstatic\r*\n$" }
// { dg-additional-options "-frust-borrowcheck -w" }

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

struct Moved {
    value: i32,
}

struct Static {
    value: i32,
}

impl Drop for Moved {
    fn drop(&mut self) {
        let msg = "moved\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for Static {
    fn drop(&mut self) {
        let msg = "static\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

fn whole_move() {
    let x = Moved { value: 1 };
    let _y = x;
}

fn static_local() {
    let _x = Static { value: 2 };
}

fn main() -> i32 {
    whole_move();
    static_local();
    0
}