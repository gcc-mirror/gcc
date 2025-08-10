// { dg-warning "unused name 'y'" "" { target *-*-* } 5 }
// { dg-warning "unused name 'z'" "" { target *-*-* } 5 }

fn main() {
    let (ref y,z) = (1i32, 2u32);
}