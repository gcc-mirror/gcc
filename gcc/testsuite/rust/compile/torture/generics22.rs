fn callee<T>(t: (T, bool)) -> i32 {
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    32
}

fn caller(t: i32) -> i32 {
    callee((t, false))
}

fn main() {
    let a;
    a = caller(123);
}
