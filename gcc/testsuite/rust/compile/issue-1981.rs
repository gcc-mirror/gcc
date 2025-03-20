#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
        pub fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
        pub fn move_val_init<T>(dst: *mut T, src: T);
        pub fn uninit<T>() -> T;
    }
}

mod ptr {
    #[lang = "const_ptr"]
    impl<T> *const T {
        pub unsafe fn offset(self, count: isize) -> *const T {
            crate::intrinsics::offset(self, count)
        }
    }

    #[lang = "mut_ptr"]
    impl<T> *mut T {
        pub unsafe fn offset(self, count: isize) -> *mut T {
            crate::intrinsics::offset(self, count) as *mut T
        }
    }

    pub unsafe fn swap_nonoverlapping<T>(x: *mut T, y: *mut T, count: usize) {
        let x = x as *mut u8;
        let y = y as *mut u8;
        let len = crate::mem::size_of::<T>() * count;
        swap_nonoverlapping_bytes(x, y, len)
    }

    pub unsafe fn swap_nonoverlapping_one<T>(x: *mut T, y: *mut T) {
        // For types smaller than the block optimization below,
        // just swap directly to avoid pessimizing codegen.
        if crate::mem::size_of::<T>() < 32 {
            let z = read(x);
            crate::intrinsics::copy_nonoverlapping(y, x, 1);
            write(y, z);
        } else {
            swap_nonoverlapping(x, y, 1);
        }
    }

    pub unsafe fn write<T>(dst: *mut T, src: T) {
        crate::intrinsics::move_val_init(&mut *dst, src)
    }

    pub unsafe fn read<T>(src: *const T) -> T {
        let mut tmp: T = crate::mem::uninitialized();
        crate::intrinsics::copy_nonoverlapping(src, &mut tmp, 1);
        tmp
    }

    unsafe fn swap_nonoverlapping_bytes(x: *mut u8, y: *mut u8, len: usize) {
        struct Block(u64, u64, u64, u64);
        struct UnalignedBlock(u64, u64, u64, u64);

        let block_size = crate::mem::size_of::<Block>();

        // Loop through x & y, copying them `Block` at a time
        // The optimizer should unroll the loop fully for most types
        // N.B. We can't use a for loop as the `range` impl calls `mem::swap` recursively
        let mut i = 0;
        while i + block_size <= len {
            // Create some uninitialized memory as scratch space
            // Declaring `t` here avoids aligning the stack when this loop is unused
            let mut t: Block = crate::mem::uninitialized();
            let t = &mut t as *mut _ as *mut u8;
            let x = x.offset(i as isize);
            let y = y.offset(i as isize);

            // Swap a block of bytes of x & y, using t as a temporary buffer
            // This should be optimized into efficient SIMD operations where available
            crate::intrinsics::copy_nonoverlapping(x, t, block_size);
            crate::intrinsics::copy_nonoverlapping(y, x, block_size);
            crate::intrinsics::copy_nonoverlapping(t, y, block_size);
            i += block_size;
        }

        if i < len {
            // Swap any remaining bytes
            let mut t: UnalignedBlock = crate::mem::uninitialized();
            let rem = len - i;

            let t = &mut t as *mut _ as *mut u8;
            let x = x.offset(i as isize);
            let y = y.offset(i as isize);

            crate::intrinsics::copy_nonoverlapping(x, t, rem);
            crate::intrinsics::copy_nonoverlapping(y, x, rem);
            crate::intrinsics::copy_nonoverlapping(t, y, rem);
        }
    }
}

mod mem {
    extern "rust-intrinsic" {
        pub fn transmute<T, U>(_: T) -> U;
        pub fn size_of<T>() -> usize;
    }

    pub fn swap<T>(x: &mut T, y: &mut T) {
        unsafe {
            crate::ptr::swap_nonoverlapping_one(x, y);
        }
    }

    pub fn replace<T>(dest: &mut T, mut src: T) -> T {
        swap(dest, &mut src);
        src
    }

    pub unsafe fn uninitialized<T>() -> T {
        crate::intrinsics::uninit()
    }
}

trait Step {
    fn replace_zero(&mut self) -> Self;
}

impl Step for i32 {
    fn replace_zero(&mut self) -> Self {
        crate::mem::replace(self, 0)
    }
}

fn main() -> i32 {
    let a = 123;
    a.replace_zero();
    a
}
