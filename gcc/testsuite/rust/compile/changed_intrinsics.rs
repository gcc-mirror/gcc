extern "rust-intrinsic" { // { dg-error "intrinsics are subject to change." "" { target *-*-* }  }
    fn foo(); 
}

fn main() {
    unsafe {
        foo();
    }
}
