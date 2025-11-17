// { dg-additional-options "-frust-unused-check-2.0" }
struct Point { x: i32, y: i32 }
// { dg-warning "field is never read: .x." "" { target *-*-* } .-1 }
// { dg-warning "field is never read: .y." "" { target *-*-* } .-2 }

pub fn main() -> (i32, i32){
    let p = Point { x: 1, y: 2 };

    match p {
        Point { mut x, mut y } => {
// { dg-warning "unused mut .x." "" { target *-*-* } .-1 }
// { dg-warning "unused mut .y." "" { target *-*-* } .-2 }
            return (x,y)
        }
    }
}

