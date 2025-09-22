// { dg-options "-w" }
struct S {
    x: i32,
    y: i32,
}

fn main() {
    let s = S{x: 1, y: 2};
    match s {
        S{x: 1, ..} => {}
    }
}
