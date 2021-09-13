// { dg-do compile }
// { dg-require-named-sections "" }

import gcc.attributes;

@section("types")
struct S {} // { dg-warning ".section. attribute does not apply to types" }

@attribute("section", 123)
int f1(); // { dg-error "section attribute argument not a string constant" }

int f2(@section("param") int a) // { dg-error "section attribute not allowed for .a." }
{
    @section("local") int v; // { dg-error "section attribute cannot be specified for local variables" }
    return a;
}

@section("c1") void conflict(); 
@section("c2") void conflict(); // { dg-error "section of .conflict. conflicts with previous declaration" }

@section("c3")
@section("c4")
void conflict2(); // { dg-error "section of .conflict2. conflicts with previous declaration" }
