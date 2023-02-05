// { dg-excess-errors "accessing value of"  }
#![feature(intrinsics)]

mod mem {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_transmute", since = "1.46.0")]
        fn transmute<T, U>(_: T) -> U;
    }
}

pub static FOO: () = unsafe {
    let illegal_ptr2int: usize = mem::transmute(&());
    let _copy = illegal_ptr2int;
};
