// Test bits/std.cc exports everything it should.
// This is done using a plugin to walk std namespace and
// its child namespaces, looking for decls with non-uglified
// names which aren't deprecated and are not exported.
// { dg-do link }
// { dg-options "-O0 -std=c++26 -fmodules -freflection -fcontracts -x c++-system-module bits/std.cc -x none" }
// { dg-skip-if "Target libm lacks standard C99/C11 mathematical types required by std module" { hppa*-*-hpux* } }

int
main ()
{
}
