
extern "rust-intrinsic" { //{ dg-error "intrinsics are subject to change." "" { target *-*-* }  }
    pub fn sqrtf32(x: f32) -> f32;
}

fn main() {
}
