#[lang = "sized"]
pub trait Sized {}

fn test<T>(a: T) -> T {
    a
}

fn main() {
    let a;
    a = test(123);
    let aa: i32 = a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b;
    b = test::<u32>(456);
    let bb: u32 = b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
