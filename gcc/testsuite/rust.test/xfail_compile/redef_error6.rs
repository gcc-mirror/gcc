// { dg-excess-errors "Noisy error and debug" }
struct Foo<T>(T, usize);

impl Foo<i32> {
    fn test() -> i32 {
        123
    }

    fn test(self) -> i32 {
        // { dg-error "redefined multiple times" "" { target *-*-* } .-1 }
        self.0
    }
}

fn main() {}
