const L: usize = 3;

fn main() {
    let p = Printer {};
    p.print();
}

trait Print<const N: usize> {
    fn print(&self) -> usize {
        3
    }
}

struct Printer {}
impl Print<L> for Printer {}
// { dg-error "generic item takes at most 1 type arguments but 1 were supplied" "" { target *-*-* } .-1 }
