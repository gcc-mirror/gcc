extern "rust-intrinsic" {
    pub fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

#[lang = "const_ptr"]
impl<T> *const T {
    pub const unsafe fn offset(self, count: isize) -> *const T {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        unsafe { offset(self, count) }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        unsafe { self.offset(count as isize) }
    }
}
