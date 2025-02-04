mod core {
    mod cmp {
        #[lang = "eq"]
        pub trait PartialEq<Rhs: ?Sized = Self> {
            fn eq(&self, other: &Rhs) -> bool;

            fn ne(&self, other: &Rhs) -> bool {
                !self.eq(other)
            }
        }

        pub trait Eq: PartialEq<Self> {
            fn assert_receiver_is_total_eq(&self) {}
        }
    }
}

#[lang = "phantom_data"]
struct PhantomData<T>;

#[lang = "sized"]
trait Sized {}

#[lang = "structural_peq"]
trait StructuralPartialEq {}

#[lang = "structural_teq"]
trait StructuralEq {}

#[derive(PartialEq)]
struct NotEq;

#[derive(Eq, PartialEq)] // { dg-error "bounds not satisfied for NotEq .Eq." }
struct Container(NotEq);

// #[derive(Eq)]
// struct Foo { a: i32 }
// #[derive(Eq)]
// struct Bar(i32);

// #[derive(Eq)]
// enum Baz {
//     A,
//     B(i32),
//     C { a: i32 }
// }

// #[derive(Eq)]
// union Qux {
//     a: i32,
//     b: i64,
// }
