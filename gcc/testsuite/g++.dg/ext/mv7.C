// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "" }

__attribute__((target ("default")))
void foo (void)	// { dg-error "previously defined here" }
{
}

__attribute__((target (128)))
void foo (void) // { dg-error "(not a string|redefinition)" }
{
}
