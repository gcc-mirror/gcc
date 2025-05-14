#[derive(Default)]
struct Foo { _a: i32, _b: i64, _c: u8 }

#[lang = "sized"]
trait Sized {}

mod core {
    mod default {
        trait Default: Sized {
            fn default() -> Self;
        }

        impl Default for i32 {
            fn default() -> Self { 0 }
        }

        impl Default for i64 {
            fn default() -> Self { 27 }
        }

        impl Default for u8 {
            fn default() -> Self { 18 }
        }
    }
}

fn main() {
    let _ = Foo::default();
}
