// { dg-do compile }
// Contributed by David Daney <daney at gcc dot gnu dot org>
// PR c++/14181: Cryptic error message for ill-formed new expressions

void f1(void)
{
   (void)new (char*)[10];  // { dg-error "parenthesized|parentheses" }
   (void)new char*[10];
}
