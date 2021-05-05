// { dg-excess-errors "Noisy error and debug" }

// #407
pub fn loopy()  {
    let mut a = 1;
    loop {
	if a < 40 {
	    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
	} else {
	    break;
	}
    }
}
