fn main() {
    let b = 15;
    let c = *(&b as *const i32); // { dg-error "dereference of raw pointer" }
}
