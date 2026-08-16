// { dg-output "^drop 1\r*\ndrop 2\r*\ndrop 3\r*\n$" }
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

fn take(_value: Droppable) {}

fn move_parameter(value: Droppable) {
    let _moved = value;
}

fn make() -> Droppable {
    let value = Droppable { value: 3 };
    value
}

fn main() -> i32 {
    let value = Droppable { value: 1 };
    take(value);

    move_parameter(Droppable { value: 2 });

    let _returned = make();

    0
}
