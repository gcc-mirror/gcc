extern "rust-intrinsic" {
    fn not_an_intrinsic();
    fn atomic_load(); // { dg-message "sorry, unimplemented: intrinsic .atomic_load. is not yet implemented" }
}

fn main() {
    unsafe { not_an_intrinsic() }; // { dg-error "unknown builtin intrinsic: not_an_intrinsic" }
    unsafe { atomic_load() };
}
