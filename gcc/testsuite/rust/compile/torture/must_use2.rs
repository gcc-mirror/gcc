trait A {
    #[must_use]
    fn test() -> i32;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

struct S;
impl A for S {
    fn test() -> i32 {
        123
    }
}

fn main() {
    S::test();
    // { dg-warning "ignoring return value of" "" { target *-*-* } .-1 }
}
