// { dg-additional-options "-w" }
// { dg-output "t1sz=5 t2sz=10\r*" }
#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod mem {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_transmute", since = "1.46.0")]
        fn transmute<T, U>(_: T) -> U;
    }
}

extern "C" {
    fn printf(s: *const i8, ...);
}

struct FatPtr<T> {
    data: *const T,
    len: usize,
}

pub union Repr<T> {
    rust: *const [T],
    rust_mut: *mut [T],
    raw: FatPtr<T>,
}

impl<T> [T] {
    pub const fn len(&self) -> usize {
        unsafe { Repr { rust: self }.raw.len }
    }
}

impl str {
    pub const fn len(&self) -> usize {
        self.as_bytes().len()
    }

    pub const fn as_bytes(&self) -> &[u8] {
        unsafe { mem::transmute(self) }
    }
}

fn main() -> i32 {
    let t1: &str = "TEST1";
    let t2: &str = &"TEST_12345";

    let t1sz = t1.len();
    let t2sz = t2.len();

    unsafe {
        let a = "t1sz=%i t2sz=%i\n";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, t1sz as i32, t2sz as i32);
    }

    0
}
