// { dg-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]

struct Expr<const N: u32>;

trait Trait0 {
    fn required(
        _: Expr<
            {
                trait Trait0 {
                    fn required();
                }

                0
            },
        >,
    );
}
