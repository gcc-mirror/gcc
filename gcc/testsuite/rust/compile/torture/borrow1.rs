fn main() {
    let a: i32;
    a = 123;

    let b: &i32;
    b = &a;

    let aa;
    aa = 456;
    let bb: &_;
    bb = &a;

    let aaa;
    aaa = 123;
    let bbb;
    bbb = &aaa;
}
