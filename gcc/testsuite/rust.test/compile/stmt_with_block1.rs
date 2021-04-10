fn test(x: i32) -> i32 {
    if x > 1 { 1 } else { 2 };
    if x > 1 { 1; } else { 2; }

    { 3; }
    { 3 };

    { 3 }
}

fn main() {
    let a = test(0); // { dg-warning "unused name" }
}
