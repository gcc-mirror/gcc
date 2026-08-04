#![feature(no_core)]
#![feature(lang_items)]
#![feature(rustc_attrs)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "copy"]
trait Copy {}

#[derive(Copy)]
pub struct SadWrap<T = ()>(T);
