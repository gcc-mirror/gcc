#![feature(no_core, lang_items, box_syntax)]
#![no_core]

extern "C" {
    fn malloc(size: usize) -> *mut u8;
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "owned_box"]
pub struct Box<T: ?Sized>(*mut T);

#[lang = "exchange_malloc"]
pub unsafe fn exchange_malloc(size: usize, _align: usize) -> *mut u8 {
    malloc(size)
}

impl<T> Box<T> {
    pub fn new(x: T) -> Box<T> {
        box x
    }
}

pub trait Animal {
    fn speak(&self) -> i32;
}

pub struct Dog {
    pub code: i32,
}

impl Animal for Dog {
    fn speak(&self) -> i32 {
        self.code
    }
}

fn main() -> i32 {
    let dog1 = Dog { code: 111 };

    let animal_ref: &dyn Animal = &dog1;

    let box_of_ref: Box<&dyn Animal> = Box::new(animal_ref);

    box_of_ref.speak() - 111
}
