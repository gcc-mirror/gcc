// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=95173
// { dg-do compile }
// { dg-options "-Wattributes" }

import gcc.attribute;

@attribute("foo") // { dg-warning "unknown attribute .foo." }
void f95173() 
{
}
