// { dg-options "-w" }
#[lang = "sized"]
pub trait Sized {}

fn test<T>(x: *mut T) {
    let x = x as *mut u8;
}
