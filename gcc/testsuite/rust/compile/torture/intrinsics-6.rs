#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    pub fn unchecked_add<T>(x: T, y: T) -> T;
    pub fn unchecked_sub<T>(x: T, y: T) -> T;
    pub fn unchecked_mul<T>(x: T, y: T) -> T;
    pub fn unchecked_div<T>(x: T, y: T) -> T;
    pub fn unchecked_rem<T>(x: T, y: T) -> T;
    pub fn unchecked_shl<T>(x: T, y: T) -> T;
    pub fn unchecked_shr<T>(x: T, y: T) -> T;
}

fn main() -> i32 {
    let zero0 = unsafe { (1 + 5) - unchecked_add(1, 5) };
    let zero1 = unsafe { (1 - 5) - unchecked_sub(1, 5) };
    let zero2 = unsafe { (1 * 5) - unchecked_mul(1, 5) };
    let zero3 = unsafe { (1 / 5) - unchecked_div(1, 5) };
    let zero4 = unsafe { (1 % 5) - unchecked_rem(1, 5) };
    let zero5 = unsafe { (1 << 5) - unchecked_shl(1, 5) };
    let zero6 = unsafe { (1 >> 5) - unchecked_shr(1, 5) };

    zero0 + zero1 + zero2 + zero3 + zero4 + zero5 + zero6
}
