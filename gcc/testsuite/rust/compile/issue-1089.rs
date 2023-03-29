// { dg-additional-options "-w" }
pub mod test_mod;

fn main() {
    let a = test_mod::Test(123);
}
