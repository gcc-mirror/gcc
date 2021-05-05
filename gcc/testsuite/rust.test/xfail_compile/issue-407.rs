// { dg-excess-errors "Noisy error and debug" }

// This already worked before the #409 code changes.
fn test()  {
    let mut a = 1;
    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
}
