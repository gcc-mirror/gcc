// { dg-additional-options -frust-mangling=v0 }
#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn main() {
    // { dg-final { scan-assembler "_R.*NC.*NvC.*10v0_mangle24main.*0" } }
    let closure_annotated = |i: i32| -> i32 { i + 1 };
    let _ = closure_annotated(0) - 1;
}
