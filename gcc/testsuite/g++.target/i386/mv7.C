// { dg-do compile }
// { dg-options "" }

__attribute__((target ("default")))
void foo (void)	// { dg-message "previously defined here" }
{
}

__attribute__((target (128)))
void foo (void) // { dg-error "(not a string|redefinition)" }
{
}
