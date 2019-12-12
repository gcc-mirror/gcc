// { dg-do compile }
// { dg-options "" }

__attribute__((target (11,12)))
void foo (void) // { dg-error "not a string" }
{
}
