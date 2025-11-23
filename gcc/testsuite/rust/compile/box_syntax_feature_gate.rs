// { dg-options "-frust-compile-until=lowering" }
#![feature(lang_items)]
#[lang = "owned_box"]
pub struct Box<T>;

fn main() {
    let x: Box<_> = box 1; //{ dg-error "box expression syntax is experimental." "" { target *-*-* }  }
}
