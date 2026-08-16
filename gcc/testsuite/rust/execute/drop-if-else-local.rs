// { dg-output "^drop 1\r*\nafter\r*\ndrop 2\r*\nafter\r*\n$" }
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

struct Droppable {
    value: i32,
}

impl Drop for Droppable {
    fn drop(&mut self) {
        let msg = "drop %d\n\0" as *const str as *const i8;
        unsafe {
            printf(msg, self.value);
        }
    }
}

fn droppable(value: i32) -> Droppable {
    Droppable { value }
}

fn test(condition: bool) {
    if condition {
        let _value = droppable(1);
    } else {
        let _value = droppable(2);
    }

    let msg = "after\n\0" as *const str as *const i8;
    unsafe {
        printf(msg);
    }
}

fn main() -> i32 {
    test(true);
    test(false);
    0
}
