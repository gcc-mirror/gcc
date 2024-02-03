#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

struct Foo<'a, 'b: 'a> {
    x: &'a i32,
    y: &'a i32,
    a: &'b i32,
    q: &'a [&'b i32],
}

pub fn test<'x, 'y>(f: Foo<'x, 'y, ()>) {
    // { dg-error "generic item takes at most 0 type arguments but 1 were supplied" "" { target *-*-* } .-1 }
    let x = 5;
    let y = 6;
    let z = 7;
    type F<'a, 'b> = fn(&'a i32, &'b i32) -> i32;
    let f = Foo {
        x: &x,
        y: &y,
        a: &z,
        q: &[&x, &y],
    };
}
