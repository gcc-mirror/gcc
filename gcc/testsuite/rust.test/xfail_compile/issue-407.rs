// { dg-excess-errors "failed to parse" }
fn test()  {
    let mut a = 1;
    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
}
