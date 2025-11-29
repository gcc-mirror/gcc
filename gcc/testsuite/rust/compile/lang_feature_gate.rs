#[lang = "sized"] // { dg-error "lang items are subject to change. add .#!.feature.lang_items... to the crate attributes to enable" }
pub trait Sized {}
