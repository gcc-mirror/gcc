#[lang = "sized"]
pub trait Sized {}

trait Valuable {
    const VALUE: i32;
}

struct Something;

macro_rules! implement {
    () => {
        const VALUE: i32 = 18;
    };
}

impl Valuable for Something {
    implement!();
}

fn main() -> i32 {
    Something::VALUE - 18
}
