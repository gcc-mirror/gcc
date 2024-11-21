// { dg-options "-w" }
pub enum X {}

pub fn foo(x: X) {
    let _a: i32 = match x {};
}

pub fn main() {}
