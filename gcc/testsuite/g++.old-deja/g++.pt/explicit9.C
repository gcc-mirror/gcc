// { dg-do assemble  }
// GROUPS passed templates
void foo(int);

void bar()
{
  foo<int>(3); // { dg-error "" } foo is not a template.
}
