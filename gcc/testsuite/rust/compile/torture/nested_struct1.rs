struct Point {
    x: f64,
    y: f64,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

struct Rectangle {
    p1: Point,
    p2: Point,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let p1 = Point { x: 0.0, y: 0.0 };
    let p2 = Point { x: 2.0, y: 4.0 };
    let rect = Rectangle { p1, p2 };

    let a = rect.p1.x;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
