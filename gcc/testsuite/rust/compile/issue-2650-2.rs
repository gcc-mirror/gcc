// { dg-additional-options "-frust-edition=2015" }

pub async fn a() -> u32 { // { dg-error "'async fn' is not permitted in Rust 2015" }
    1
}
