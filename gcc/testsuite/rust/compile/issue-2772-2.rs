// { dg-options "-w" }
#[lang = "sized"]
pub trait Sized {}

struct Pair<'a, T, U>
where
    T: 'a,
    U: 'a,
{
    left: T,
    right: U,
}

pub fn test<'a>() {
    let a: i32 = 50;
    let x = Pair::<&'_ _, &'_ _> {
        left: &&a,
        right: &a,
    };
}
