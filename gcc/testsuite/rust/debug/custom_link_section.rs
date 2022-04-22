#[link_section = ".universe"]
fn not_in_text() -> i32 {
    42
}

fn main() -> i32 {
// { dg-do compile }
// { dg-options "-gdwarf-5 -dA -w" }
    not_in_text();
// { dg-final { scan-assembler ".universe" } } */

    0
}
