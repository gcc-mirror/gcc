// { dg-warning "field is never read: .a." "" { target *-*-* } .-1 }
struct Foo { a: i32 }

fn main() {
    let a = Foo { a: 15 };
    
    match a {
        b => { }
    }
}