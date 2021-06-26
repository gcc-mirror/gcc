fn foo() -> i32 {
    1
}
    
fn main() {
    let _a = loop {
        break foo();
    };
}
    