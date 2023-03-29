fn test(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let a = test(1, true);
    // { dg-error "expected .i32. got .bool." "" { target *-*-* } .-1 }
    // { dg-error "Type Resolution failure on parameter" "" { target *-*-* } .-2 }
}
