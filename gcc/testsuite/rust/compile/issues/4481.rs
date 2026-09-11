#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

trait Trait<T: NewTrait> {}

fn foo<T>()
where
    T: Trait<for<'b> fn(&'b u32)>, // { dg-error {bounds not satisfied for abi:rust fnptr .& u32 ,. -> .. .NewTrait. is not satisfied \[E0277\]} }
{
}

impl<'a> Trait<fn(&'a u32)> for () {} // { dg-error {bounds not satisfied for abi:rust fnptr .& u32 ,. -> .. .NewTrait. is not satisfied \[E0277\]} }

fn main() {
    foo::<()>();
}

trait NewTrait {
    type Assoc;
    const C: Self::Assoc;
    fn f(&self) -> Self::Assoc;
}
