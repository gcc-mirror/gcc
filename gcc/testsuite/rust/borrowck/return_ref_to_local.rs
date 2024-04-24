// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

pub fn return_ref_to_local() -> &'static i32 { // { dg-error "Found loan errors in function return_ref_to_local" }
    let x = 0;
    &x //~ ERROR
}
