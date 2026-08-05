#![feature(no_core, lang_items, box_syntax, intrinsics)]
#![no_core]

extern "C" {
    fn malloc(size: usize) -> *mut u8;
}

extern "rust-intrinsic" {
    fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

#[lang = "index"]
pub trait Index<Idx> {
    type Output: ?Sized;
    fn index(&self, index: Idx) -> &Self::Output;
}

impl Index<usize> for [i32] {
    type Output = i32;

    fn index(&self, index: usize) -> &i32 {
        unsafe {
            let ptr = self as *const [i32] as *const i32;
            &*offset(ptr, index as isize)
        }
    }
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "owned_box"]
pub struct Box<T: ?Sized>(*mut T);

#[lang = "exchange_malloc"]
pub unsafe fn em(size: usize, _align: usize) -> *mut u8 {
    malloc(size)
}

impl<T> Box<T> {
    pub fn new(x: T) -> Box<T> {
        box x
    }
}

pub struct X { data: i32 }

pub trait A {
    fn a(&self) -> i32;
}

impl A for X {
    fn a(&self) -> i32 {
        self.data
    }
}
pub fn main() -> i32 {
    let x = X { data: 44 };
    let y = X { data: 22 };
    let z : [i32; 3] = [1, 2, 3];
    let w : (i32, i32) = (10, 20);

    let a : Box<dyn A> = Box::new(x);
    let b : Box<X> = Box::new(y);
    let c : Box<[i32; 3]> = Box::new(z);
    let d : Box<(i32, i32)> = Box::new(w);
    let e : Box<[i32]> = Box::new(z);

    a.a() - 2 * b.data // 44 - 2 * 22
    +
    c[0] + c[1] - c[2] // 1 + 2 - 3
    +
    2 * d.0 - d.1 // 2 * 10 - 20
    +
    e[0] + e[1] - e[2] // 1 + 2 - 3
}
