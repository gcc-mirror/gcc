// { dg-output "3\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn f<F: FnOnce(i32) -> i32>(g: F) {
    let call = g(1);
    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, call);
    }
}

pub fn main() -> i32 {
    let a = |i: i32| {
        let b = i + 2;
        b
    };
    f(a);
    0
}
