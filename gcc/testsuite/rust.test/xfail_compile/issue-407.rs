// { dg-excess-errors "Noisy error and debug" }

fn test()  {
    let mut a = 1;
    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
}
