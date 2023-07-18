/* { dg-output "3\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

trait FnLike<A, R> {
    fn call(&self, arg: A) -> R;
}

type FnObject<'b> = dyn for<'a> FnLike<&'a isize, &'a isize> + 'b;

struct Identity;

impl<'a, T> FnLike<&'a T, &'a T> for Identity {
    fn call(&self, arg: &'a T) -> &'a T {
        // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
        arg
    }
}

fn call_repeatedly(f: &FnObject) {
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
    call_repeatedly(&Identity);

    0
}
