#![feature(no_core)]
#![no_core]

pub fn s(num: i32) -> i32 {
    r#_ * num /* { dg-error "not a valid raw identifier" } */
}
