// Build don't link:
// GROUPS passed templates
void foo(int);

void bar()
{
  foo<int>(3); // ERROR - foo is not a template.
}
