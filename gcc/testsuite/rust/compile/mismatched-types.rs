// ErrorCode::E0308
#![allow(unused)]
fn main() {
    fn plus_one(x: i32) -> i32 { 
        x + 1 
    }
    plus_one("Not a number");       // { dg-error "mismatched types, expected .i32. but got .& str." }
    let x: f32 = "Not a float";     // { dg-error "mismatched types, expected .f32. but got .& str." }
}
