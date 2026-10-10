#![feature(no_core)]
#![no_core]

#![feature(intrinsics)]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone: Sized {
    fn clone(&self) -> Self;

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

mod impls {
    use super::Clone;

    macro_rules! impl_clone {
        ($($t:ty)*) => {
            $(
                impl Clone for $t {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
            )*
        }
    }

    impl_clone! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

#[lang = "copy"]
pub trait Copy: Clone {
    // Empty.
}

mod copy_impls {
    use super::Copy;

    macro_rules! impl_copy {
        ($($t:ty)*) => {
            $(
                impl Copy for $t {}
            )*
        }
    }

    impl_copy! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

extern "rust-intrinsic" {
    pub fn atomic_cxchg<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchg_acq<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchg_rel<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchg_acqrel<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchg_relaxed<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchgweak<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchgweak_acq<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchgweak_rel<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchgweak_acqrel<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
    pub fn atomic_cxchgweak_relaxed<T: Copy>(dst: *mut T, old: T, new: T) -> (T, bool);
}

fn main() -> u32 {
    unsafe {
        let mut src = -7i8;

        let result = atomic_cxchg(&mut src, -7, 9);
        if result.0 != -7 || !result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchg(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchgweak(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        // Retry weak compare-exchange when it fails spuriously.
        loop {
            let result = atomic_cxchgweak(&mut src, 9, -7);
            if result.0 != 9 {
                return 1;
            }
            if result.1 {
                if src != -7 {
                    return 1;
                }
                break;
            }
            if src != 9 {
                return 1;
            }
        }

        let result = atomic_cxchg_acq(&mut src, -7, 9);
        if result.0 != -7 || !result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchg_acq(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchgweak_acq(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        loop {
            let result = atomic_cxchgweak_acq(&mut src, 9, -7);
            if result.0 != 9 {
                return 1;
            }
            if result.1 {
                if src != -7 {
                    return 1;
                }
                break;
            }
            if src != 9 {
                return 1;
            }
        }

        let result = atomic_cxchg_rel(&mut src, -7, 9);
        if result.0 != -7 || !result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchg_rel(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchgweak_rel(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        loop {
            let result = atomic_cxchgweak_rel(&mut src, 9, -7);
            if result.0 != 9 {
                return 1;
            }
            if result.1 {
                if src != -7 {
                    return 1;
                }
                break;
            }
            if src != 9 {
                return 1;
            }
        }

        let result = atomic_cxchg_acqrel(&mut src, -7, 9);
        if result.0 != -7 || !result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchg_acqrel(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchgweak_acqrel(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        loop {
            let result = atomic_cxchgweak_acqrel(&mut src, 9, -7);
            if result.0 != 9 {
                return 1;
            }
            if result.1 {
                if src != -7 {
                    return 1;
                }
                break;
            }
            if src != 9 {
                return 1;
            }
        }

        let result = atomic_cxchg_relaxed(&mut src, -7, 9);
        if result.0 != -7 || !result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchg_relaxed(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        let result = atomic_cxchgweak_relaxed(&mut src, 3, 11);
        if result.0 != 9 || result.1 || src != 9 {
            return 1;
        }

        loop {
            let result = atomic_cxchgweak_relaxed(&mut src, 9, -7);
            if result.0 != 9 {
                return 1;
            }
            if result.1 {
                if src != -7 {
                    return 1;
                }
                break;
            }
            if src != 9 {
                return 1;
            }
        }
    }

    0
}
