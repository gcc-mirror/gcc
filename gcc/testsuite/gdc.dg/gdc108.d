// https://bugzilla.gdcproject.org/show_bug.cgi?id=108
// { dg-do compile }

import gcc.attributes;

@attribute("always_inline")
void forceinline108()
{
}

@attribute("noinline")
void noinline108()
{
}

@attribute("flatten")
void flatten108()
{
}
