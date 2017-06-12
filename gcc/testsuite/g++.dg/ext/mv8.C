// { dg-do compile { target i?86-*-* x86_64-*-* powerpc*-*-* aarch64*-*-* } }
// { dg-options "" }

__attribute__((target (11,12)))
void foo (void) // { dg-error "not a string" }
{
}
