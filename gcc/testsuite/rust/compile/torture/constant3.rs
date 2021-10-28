fn main() {
    const A: [i32; 3] = [1, 2, 3];
    const B: i32 = A[1];
    const C: usize = 42;
    const D: i32 = 7;

    let _a = C;
    let _b: [i32; C] = [0; C];
    let _c = B + D;
}
