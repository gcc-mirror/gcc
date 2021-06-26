fn foo() -> i32 {
    1
}


fn main() {
    let _a: [i32; 1] = [foo()];
}