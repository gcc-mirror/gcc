// { dg-options "-frust-compile-until=lowering" }
#[lang = "owned_box"]
pub struct Box<T>;

fn main() {
    let x: Box<_> = box 1; //{ dg-error "box expression syntax is experimental." "" { target *-*-* }  }
}
