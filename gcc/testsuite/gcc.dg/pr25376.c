/* PR25376.  Verify that a named section is honored.  */
/* { dg-require-named-sections "" } */

void simple (void) __attribute__((section("my_named_section")));
void simple (void)
{
}

/* { dg-final { scan-assembler "my_named_section" } } */
/* { dg-final { scan-assembler-symbol-section {simple$} {^\.?my_named_section|simple\[DS\]|^\"\.opd\"} } } */
