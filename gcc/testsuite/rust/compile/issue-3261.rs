// { dg-options "-w" }
fn main() {
    let a: i8 = 50;
    let b = a as f32;
    let c = a as f64;

    let a: i16 = 1337;
    let b = a as f32;
    let c = a as f64;

    let a: i32 = 1337;
    let b = a as f32;
    let c = a as f64;

    let a: i64 = 1337;
    let b = a as f32;
    let c = a as f64;
}
