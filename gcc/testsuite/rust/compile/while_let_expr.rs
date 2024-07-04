// { dg-options "-fsyntax-only" }

pub enum Option<T> {
    None,
    Some(T),
}

fn main() {
    let mut x = Option::Some(3);
    let a = while let Option::Some(1) = x {
        x = Option::None;
    };
}
