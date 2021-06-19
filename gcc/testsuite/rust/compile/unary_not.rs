fn main() {
    let a: i32 = -1;
    let b: i32 = 3 - -1;
    let c: bool = !false;
    let d: i32 = !3;

    let e: f32 = !5f32; // { dg-error "cannot apply unary '!' to f32" }
    // { dg-error {failed to type resolve expression} "" { target *-*-* } .-1 }
}
