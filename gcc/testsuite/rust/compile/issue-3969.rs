#![feature(lang_items)]

#[lang = "sized"]
pub trait Sized {
    // Empty.
}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn main() {
    [(); {
        while true {
            // { dg-error ".constexpr. loop iteration count exceeds limit" "" { target *-*-* } .-1 }
            break 9;
            // { dg-error "can only .break. with a value inside a .loop. block .E0571." "" { target *-*-* } .-1 }
        }
        51
    }];

    while true {
        break (|| {
            // { dg-error "can only .break. with a value inside a .loop. block .E0571." "" { target *-*-* } .-1 }
            let while_true = 9;
        });
    }
}
