macro_rules! add {
    ($e:expr big_tok $($es:expr) big_tok *) => {
        $e + add!($($es) big_tok *)
    };
    ($e:expr) => {
        $e
    };
}

fn main() -> i32 {
    let a = add!(15 big_tok 2 big_tok 9); // 26

    a - 26
}
