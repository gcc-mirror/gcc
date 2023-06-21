#![feature(intrinsics)]

extern "rust-intrinsic" {
    fn not_an_intrinsic();
}

fn main() {
    unsafe { not_an_intrinsic() }; // { dg-error "unknown builtin intrinsic: not_an_intrinsic" }
}
