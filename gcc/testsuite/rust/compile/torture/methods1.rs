struct Point {
    x: f64,
    y: f64,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

impl Point {
    fn origin() -> Point {
        Point { x: 0.0, y: 0.0 }
    }

    fn new(x: f64, y: f64) -> Point {
        Point { x: x, y: y }
    }
}

struct Rectangle {
    p1: Point,
    p2: Point,
}

impl Rectangle {
    fn from(p1: Point, p2: Point) -> Self {
        Self { p1, p2 }
    }

    fn sum_x(self) -> f64 {
        let p1 = self.p1;
        let p2 = self.p2;
        p1.x + p2.x
    }
}

fn main() {
    let p1 = Point::origin();
    let p2 = Point::new(3.0, 4.0);
    let rect = Rectangle::from(p1, p2);

    let sum = rect.sum_x();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
