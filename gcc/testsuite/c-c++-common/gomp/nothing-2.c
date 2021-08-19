#pragma omp nothing ,		/* { dg-error "expected end of line before" } */
#pragma omp nothing asdf	/* { dg-error "expected end of line before" } */
