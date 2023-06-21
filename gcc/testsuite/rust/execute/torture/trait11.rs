/* { dg-output "3\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

trait FnLike<A, R> {
    fn call(&self, arg: A) -> R;
}

struct S;
impl<'a, T> FnLike<&'a T, &'a T> for S {
    fn call(&self, arg: &'a T) -> &'a T {
        // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
        arg
    }
}

fn indirect<F>(f: F)
where
    F: for<'a> FnLike<&'a isize, &'a isize>,
{
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
