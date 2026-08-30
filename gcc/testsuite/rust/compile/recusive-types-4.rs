// { dg-options "-w" }

#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

struct TupleNested {
    link: (*const TupleNested,),
}

struct ArrayNested {
    links: [*const ArrayNested; 2],
}
