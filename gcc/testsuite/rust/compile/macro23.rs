macro_rules! maybe_impl {
    ($left:ident, $right:ident, $l_fn:ident, $r_fn:ident) => {
        fn $l_fn(value: T) -> Maybe<T> {
            Maybe::$left(value)
        }

        fn $r_fn() -> Maybe<T> {
            Maybe::$right
        }
    };
}

enum Maybe<T> {
    Just(T),
    Nothing,
}

impl<T> Maybe<T> {
    maybe_impl!(Just, Nothing, just, nothing);
}

fn main() {
    let _ = Maybe::just(14);
    let _: Maybe<i32> = Maybe::nothing();
}
