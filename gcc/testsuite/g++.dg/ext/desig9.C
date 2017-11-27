// { dg-do compile }

int a[2] = { [0] = 1, [1] = 2 };	// { dg-error "does not allow C99 designated initializers" }
