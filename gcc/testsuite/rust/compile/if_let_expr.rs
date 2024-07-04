#[lang = "sized"]
pub trait Sized {}

pub enum Option<T> {
    None,
    Some(T),
}

fn main() {
    let x = Option::Some(3); // { dg-warning "unused name" }
    let a = if let Option::Some(1) = x {
        1
    } else if x == Option::Some(2) {
        2
    } else if let Option::Some(y) = x {
        y
    } else {
        -1
    };
}
