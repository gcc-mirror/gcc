// { dg-options "-w" }
struct S();

fn main() {
    let s = S{};
    match s {
        S{..} => {}
    }
}
