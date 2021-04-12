struct Point {
    x: f64,
    y: f64,
}

struct Rectangle {
    p1: Point,
    p2: Point,
}

fn main() {
    let p1 = Point { x: 0.0, y: 0.0 };
    let p2 = Point { x: 2.0, y: 4.0 };
    let rect = Rectangle { p1, p2 };

    let a = rect.p1.x;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
