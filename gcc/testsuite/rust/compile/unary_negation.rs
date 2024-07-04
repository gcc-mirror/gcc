fn main() {
    let a: i32 = -1;
    let b: i32 = 3 - -1;
    let c: bool = !false;
    let d: i32 = !3;

    let e: f32 = -true; // // { dg-error "cannot apply unary - to bool" }
}
