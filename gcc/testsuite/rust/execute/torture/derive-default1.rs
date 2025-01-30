#[derive(Default)]
struct Foo { a: i32  }
#[derive(Default)]
struct Bar(i32);

#[lang = "sized"]
trait Sized {}

mod core {
    mod default {
        trait Default: Sized {
            fn default() -> Self;
        }

        impl Default for i32 {
            fn default() -> Self { 1 }
        }
    }
}

fn main() -> i32 {
    let foo = Foo::default();
    let bar = Bar::default();

    foo.a + bar.0 - 2
}
