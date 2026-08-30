// { dg-options "-w" }

#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
pub trait Sized {}

struct Node {
    value: i32,
    next: *const Node,
}

fn main() {
    let node = Node {
        value: 1,
        next: 0 as *const Node,
    };

    let _value = node.value;
    let _next = node.next;
}
