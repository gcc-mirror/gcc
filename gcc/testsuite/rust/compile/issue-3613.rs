mod m1 {
    pub enum Baz4 {
        foo1,
        foo2,
    }
}

fn bar(x: m1::foo) {
    // { dg-error "unknown reference for resolved name: .foo." "" { target *-*-* } .-1 }
    match x {
        m1::foo::foo1 => {}
        // { dg-error "failed to type resolve root segment" "" { target *-*-* } .-1 }
        m1::NodePosition::foo2 => {}
        // { dg-error "failed to type resolve root segment" "" { target *-*-* } .-1 }
    }
}

pub fn main() {}
