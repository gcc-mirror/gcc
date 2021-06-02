pub fn plus(r#break: i32, r#unsafe: i32) -> i32 { /* { dg-warning "used" } */
    r#break + r#unsafe
}