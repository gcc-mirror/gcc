// PR c++/96103
// { dg-do compile { target c++11_only } }

decltype(auto) foo () { return 4; } // { dg-error ".decltype\\(auto\\). type specifier only available" }

void
bar ()
{
  decltype(auto) i = 0; // { dg-error ".decltype\\(auto\\). type specifier only available" }
}
