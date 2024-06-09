#[macro_use]
mod foo {
    macro_rules! a {
        () => {
            15
        };
    }

    macro_rules! b {
        () => {
            14
        };
    }
}

fn main() -> i32 {
    a!() + b!() - 29
}
