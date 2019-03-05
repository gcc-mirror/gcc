// We used to stop parsing at the first top-level '}' !

} // { dg-error "expected declaration" }

float int c; // { dg-error "two or more data types" }
