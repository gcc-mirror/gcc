fn foo() -> usize {
    1
}
    
fn bar() -> [i32; 1] {
    [0]
}
    
    
        
fn main() -> () {
    let a = [10];
    let _b = a[foo()];
    let _c = bar()[foo()];
}
