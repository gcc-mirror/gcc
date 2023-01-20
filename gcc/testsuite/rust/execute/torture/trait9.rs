/* { dg-output "3\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

trait FnLike<A, R> {
    fn call(&self, arg: A) -> R;
}

struct S;
impl<T> FnLike<&T, &T> for S {
    fn call(&self, arg: &T) -> &T {
        // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
        arg
    }
}

fn indirect<F: FnLike<&isize, &isize>>(f: F) {
    let x = 3;
    let y = f.call(&x);

    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, *y);
    }
}

fn main() -> i32 {
    indirect(S);

    0
}
