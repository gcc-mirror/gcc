// { dg-options "-w" }
fn test<T>(x: *mut T) {
    let x = x as *mut u8;
}
