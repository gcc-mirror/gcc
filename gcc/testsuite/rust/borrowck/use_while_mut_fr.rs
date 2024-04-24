// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

pub fn use_while_mut_fr(x: &mut i32) -> &mut i32 {  // { dg-error "Found loan errors in function use_while_mut_fr" }
    let y = &mut *x;
    let z = x; //~ ERROR
    y
}

