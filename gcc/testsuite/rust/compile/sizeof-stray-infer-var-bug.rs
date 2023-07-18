#[lang = "sized"]
pub trait Sized {}

mod mem {
    extern "rust-intrinsic" {
        pub fn size_of<T>() -> usize;
    }
}

mod ptr {

    pub unsafe fn swap_nonoverlapping<T>(x: *mut T, y: *mut T, count: usize) {
        let x = x as *mut T;
        let y = y as *mut T;
        let len = mem::size_of::<T>() * count;
    }
}
