// { dg-options "-w" }

#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

struct Node<T> {
    value: T,
    next: *const Node<T>,
}

fn main() {
    let node = Node::<i32> {
        value: 1,
        next: 0 as *const Node<i32>,
    };
    let _value = node.value;

    let other = Node::<u32> {
        value: 2,
        next: 0 as *const Node<u32>,
    };
    let _other_value = other.value;
}
