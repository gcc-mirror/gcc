// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }
pub fn use_while_mut() {  // { dg-error "Found loan errors in function use_while_mut" }
    let mut x = 0;
    let y = &mut x;
    let z = x; //~ ERROR
    let w = y;
}