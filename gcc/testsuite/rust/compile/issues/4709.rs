// { dg-options "-w" }

#![feature(no_core)]
#![no_core]

struct Node<'a> {
    n: &'a Node<'a>,
}
