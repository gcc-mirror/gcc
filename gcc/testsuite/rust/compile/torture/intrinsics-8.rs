#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn add_with_overflow<T>(x: T, y: T) -> (T, bool);
        pub fn sub_with_overflow<T>(x: T, y: T) -> (T, bool);
        pub fn mul_with_overflow<T>(x: T, y: T) -> (T, bool);
    }
}

pub enum Option<T> {
    None,
    Some(T),
}

impl i32 {
    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        let (a, b) = self.overflowing_add(rhs);
        if b {
            Option::None
        } else {
            Option::Some(a)
        }
    }

    pub fn overflowing_add(self, rhs: Self) -> (Self, bool) {
        let (a, b) = unsafe { intrinsics::add_with_overflow(self as i32, rhs as i32) };
        (a as Self, b)
    }

    pub fn overflowing_sub(self, rhs: Self) -> (Self, bool) {
        let (a, b) = unsafe { intrinsics::sub_with_overflow(self as i32, rhs as i32) };
        (a as Self, b)
    }

    pub fn overflowing_mul(self, rhs: Self) -> (Self, bool) {
        let (a, b) = unsafe { intrinsics::mul_with_overflow(self as i32, rhs as i32) };
        (a as Self, b)
    }
}
