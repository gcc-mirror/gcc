#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub enum Ordering {
    Less,
    Equal,
    Greater,
}

pub trait Ord {
    fn cmp(&self, other: &Self) -> Ordering;
}

impl Ord for i32 {
    fn cmp(&self, _other: &i32) -> Ordering {
        Ordering::Equal
    }
}

impl Ord for ! {
    fn cmp(&self, _other: &!) -> Ordering {
        Ordering::Equal
    }
}

fn main() {
    let left: i32 = 1;
    let right: i32 = 2;
    let _ = left.cmp(&right);
}
