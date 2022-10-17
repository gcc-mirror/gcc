// PR c++/102413
// { dg-do compile { target c++11 } }

[[omp::directive(error]];	// { dg-error "expected|declare" }
