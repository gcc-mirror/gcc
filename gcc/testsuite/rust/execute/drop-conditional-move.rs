// { dg-output "^drop\r*\nafter conditional\r*\nafter conditional\r*\ndrop\r*\nafter static\r*\ndrop\r*\nafter static\r*\ndrop\r*\ndrop\r*\ndrop\r*\n$" }
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

struct AfterConditional {
    value: i32,
}

struct AfterStatic {
    value: i32,
}

impl Drop for Droppable {
    fn drop(&mut self) {
        let msg = "drop\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for AfterConditional {
    fn drop(&mut self) {
        let msg = "after conditional\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for AfterStatic {
    fn drop(&mut self) {
        let msg = "after static\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

fn conditional_move(condition: bool) {
    let x = Droppable { value: 1 };

    if condition {
        let _y = x;
    }

    let _after = AfterConditional { value: 0 };
}

fn static_after_join(condition: bool) {
    let _x = Droppable { value: 2 };

    if condition {
        let _n = 1;
    }

    let _after = AfterStatic { value: 0 };
}

fn move_on_both_branches(condition: bool) {
    let x = Droppable { value: 3 };

    if condition {
        let _y = x;
    } else {
        let _z = x;
    }
}

fn main() -> i32 {
    conditional_move(true);
    conditional_move(false);

    static_after_join(true);
    static_after_join(false);

    move_on_both_branches(true);
    move_on_both_branches(false);

    0
}