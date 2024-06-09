mod intrinsics {
    extern "rust-intrinsic" {
        pub fn assume(value: bool);
    }
}

pub fn foo(v: i32) -> i32 {
    unsafe { intrinsics::assume (v == 12); }
    v
}

pub fn main() {
}
