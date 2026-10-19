// { dg-options "-w" }

#![feature(no_core)]
#![no_core]

pub struct Edge<'a> {
    pub next: &'a Edge<'a>
}
