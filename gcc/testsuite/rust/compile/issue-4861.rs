#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

trait Searcher<'a> {
    fn haystack(&self) -> &'a str;
    fn with_lifetime<'b>(&self, value: &'b str) -> &'a str;
    fn borrow(&'a self) -> &'a str;
}

trait Other<'a, 'b> {
    fn first(&self) -> &'a str;
    fn second(&self) -> &'b str;
    fn method<'c>(&self, value: &'c str) -> &'c str;
}
