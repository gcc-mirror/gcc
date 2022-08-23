macro_rules! two {
    (2) => {
        3
    };
}

macro_rules! one {
    (1) => {{
        two!(2)
    }};
}

fn main() -> i32 {
    let a = one!(1);

    a - 3
}
