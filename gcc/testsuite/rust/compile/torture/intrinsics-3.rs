#![feature(intrinsics)]

extern "rust-intrinsic" {
    fn not_an_intrinsic();
}

fn main() {
    unsafe { not_an_intrinsic() }; // { dg-error "unrecognized intrinsic function: .not_an_intrinsic." }
}
