// { dg-warning "field is never read: .x." "" { target *-*-* } .-1 }
// { dg-warning "field is never read: .y." "" { target *-*-* } .-2 }
struct Point {
    x: u32,
    y: u32,
}

fn is_origin(p: Point) -> bool {
    match p {
        Point { x, y } => {
            if x == 0 && y == 0 {
                return true;
            }
            false
        }
        _ => false,
    }
}

fn main() -> i32 {
    let p = Point { x: 0, y: 0 };
    let q = Point { x: 0, y: 1 };
    let mut retval = 2;
    
    if is_origin(p) {
        retval -= 1;
    }
    
    if !is_origin(q) {
        retval -= 1;
    }

    retval
}
