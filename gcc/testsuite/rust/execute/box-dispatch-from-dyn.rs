#![feature(no_core, lang_items, box_syntax)]
#![no_core]

extern "C" {
    fn malloc(size: usize) -> *mut u8;
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "unsize"]
pub trait Unsize<T: ?Sized> {}

#[lang = "coerce_unsized"]
pub trait CoerceUnsized<T: ?Sized> {}

#[lang = "dispatch_from_dyn"]
pub trait DispatchFromDyn<T> {}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>;

#[lang = "exchange_malloc"]
pub unsafe fn exchange_malloc(size: usize, _align: usize) -> *mut u8 {
    malloc(size)
}

pub struct NonNull<T: ?Sized> {
    pub ptr: *const T,
}

pub struct Unique<T: ?Sized> {
    pub pointer: NonNull<T>,
    pub _marker: PhantomData<T>,
}

#[lang = "owned_box"]
pub struct Box<T: ?Sized> {
    pub inner: Unique<T>,
}

impl<T: ?Sized> Box<T> {
    pub fn new(x: T) -> Box<T> {
        box x
    }
}

#[lang = "receiver"]
pub trait Receiver {}

trait Animal {
    fn get_age(self: Box<Self>) -> i32;
}

struct Dog {
    age: i32,
}

impl Animal for Dog {
    fn get_age(self: Box<Self>) -> i32 {
        self.age
    }
}

pub fn main() -> i32 {
    let dog = Dog { age: 42 };
    let _ = dog.age;

    let coerced_box: Box<dyn Animal> = Box::new(dog);

    let result = coerced_box.get_age();

    if result == 42 {
        0
    } else {
        1
    }
}
