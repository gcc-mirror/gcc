// { dg-do compile }
// { dg-skip-if "only works for ELF targets" { *-*-darwin* *-*-aix* } }

import gcc.attributes;

@symver("type")
struct S {} // { dg-warning ".symver. attribute does not apply to types" }

@attribute("symver", 123)
int f1(); // { dg-error ".symver. attribute argument not a string constant" }

@symver("format")
int f2() // { dg-error "symver attribute argument must have format .name@nodename'" }
{ // { dg-error ".symver. attribute argument .format. must contain one or two .@." "" { target *-*-* } .-1 }
    return 0;
}

int f3(@symver("param@VER_1") int param) // { dg-warning ".symver. attribute only applies to functions and variables" }
{
    return param;
}

@symver("extern@VER_2")
extern int f4(); // { dg-error "symbol needs to be defined to have a version" }
