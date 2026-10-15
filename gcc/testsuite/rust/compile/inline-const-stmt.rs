#![feature(no_core)]
#![no_core]

fn stmt() {
    const { 1 + 1 };
    const {
        let _a = 2;
    }
    let _b = 3;
}

fn tail() -> i32 {
    const { 1 + 1 }
}

fn main() {
    stmt();
    tail();
}
