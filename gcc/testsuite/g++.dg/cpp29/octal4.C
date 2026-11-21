// P0085R5 - Oo... adding a coherent character sequence to begin octal-literals
// { dg-do compile { target c++14 } }
// { dg-options "" }

int a = 0o;		// { dg-error "unable to find numeric literal operator 'operator\"\"o'" }
int b = 0Oa;		// { dg-error "unable to find numeric literal operator 'operator\"\"Oa'" }
int c = 0O08;		// { dg-error "invalid digit" }
int d = 0o1.1;		// { dg-error "invalid prefix" }
int e = 0O0p0;		// { dg-error "unable to find numeric literal operator 'operator\"\"p0'" }
