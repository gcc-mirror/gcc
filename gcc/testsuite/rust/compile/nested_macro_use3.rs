#[macro_use]
mod num {
    #[macro_use]
    mod macros {
        macro_rules! a {
            () => ()
        }
    }

    a!();
}

fn main() -> i32 { 
    a!();
    0
}
