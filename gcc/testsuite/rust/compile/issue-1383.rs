pub fn generic_function<X>(a: X) -> X {
    a
}

fn main() -> i32 {
    let a = generic_function(123);
    a - 123
}
