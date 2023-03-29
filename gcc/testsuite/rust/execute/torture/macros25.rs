macro_rules! t {
    ($t:tt) => {
        $t
    };
}

fn frob() -> i32 {
    t!(15) + t!((14))
}

fn main() -> i32 {
    frob() - 29
}
