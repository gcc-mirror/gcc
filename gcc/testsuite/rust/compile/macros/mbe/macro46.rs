fn foo() {}

fn main() {
    macro_rules! a {
        () => {
            foo();
        };
    }

    {
        macro_rules! a {
            () => {
                bar();
            };
        }
    }

    a!();
}
