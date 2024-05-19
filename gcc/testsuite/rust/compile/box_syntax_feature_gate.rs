// { dg-options "-frust-compile-until=lowering" }

fn main() {
    let x: Box<_> = box 1; //{ dg-error "box expression syntax is experimental." "" { target *-*-* }  }
}
