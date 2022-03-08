macro_rules! add_parens {
    ($($rep:ident ( ) )*) => {
        { 0 $(+ $rep ( ))* }
    };
}

fn f() -> i32 {
    1
}

fn main() -> i32 {
    let a = add_parens!(f() f() f());

    a - 3
}
