#[lang = "sized"]
pub trait Sized {}

macro_rules! define_trait {
    ($assoc:ident, $i:item) => {
        type $assoc;

        $i
    };
}

trait DefinedThroughMacros {
    define_trait!(
        Inner,
        fn takes_inner(i: Self::Inner) -> Self::Inner {
            i
        }
    );
}
