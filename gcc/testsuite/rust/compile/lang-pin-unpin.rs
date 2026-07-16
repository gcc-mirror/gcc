#![feature(no_core, lang_items, optin_builtin_traits, negative_impls)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "unpin"]
pub auto trait Unpin {}

#[lang = "pin"]
pub struct Pin<P> {
    pointer: P,
}

impl<P> Pin<P> {
    pub fn new(pointer: P) -> Pin<P>
    where
        P: Unpin,
    {
        Pin { pointer }
    }
}

struct PhantomPinned;
impl !Unpin for PhantomPinned {}

struct PinnedStruct {
    _marker: PhantomPinned, 
}

struct NormalStruct;

fn main() {
    let _ = Pin::new(NormalStruct); 

    let pinned = PinnedStruct { _marker: PhantomPinned };
    let _ = Pin::new(pinned); // { dg-error "bounds not satisfied for PinnedStruct .Unpin. is not satisfied .E0277." }
}
