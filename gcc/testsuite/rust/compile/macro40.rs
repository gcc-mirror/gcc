// { dg-additional-options "-w" }

macro_rules! t {
    () => {
        i32
    };
}

macro_rules! s {
    () => {
        *const i8
    };
}

extern "C" {
    fn printf(s: s!(), ...);
}

fn square(arg: t!()) -> t!() {
    let input: t!() = arg;

    input * input
}

trait Trait {
    fn f() -> t!();
    fn g(arg: t!());
}

struct Wrapper {
    inner: t!(),
}

impl Trait for Wrapper {
    fn f() -> t!() {
        1
    }

    fn g(arg: t!()) {}
}

fn id<T>(arg: T) -> T {
    arg
}

fn main() {
    id::<t!()>(15);
}
