// { dg-do compile }
module asm1;

void parse1()
{
    asm
    {
        ""h;    // { dg-error "found 'h' when expecting ':'" }
    }
}

void parse2()
{
    asm 
    {
        "" : : "g" (1 ? 2 : 3);
        "" : : "g" (1 ? 2 : :) 3;
        // { dg-error "expression expected, not ':'" "" { target *-*-* } .-1 }
        // { dg-error "expected constant string constraint for operand" "" { target *-*-* } .-2 }
    }
}

void parse3()
{
    asm { "" [; }
    // { dg-error "expression expected, not ';'" "" { target *-*-* } .-1 }
    // { dg-error "found 'End of File' when expecting ','" "" { target *-*-* } .-2 }
    // { dg-error "found 'End of File' when expecting ']'" "" { target *-*-* } .-3 }
    // { dg-error "found 'End of File' when expecting ';'" "" { target *-*-* } .-4 }
}

void parse4()
{
    int expr;
    asm
    {
        "%name" : [name] string (expr); // { dg-error "expected constant string constraint for operand, not 'string'" }
    }
}

void semantic1()
{
    {
        int one;
    L1:
        ;
    }
    asm { "" : : : : L1, L2; }
    // { dg-error "'goto' skips declaration of variable 'asm1.semantic1.one'" "" { target *-*-* } .-1 }
    // { dg-error "'goto' skips declaration of variable 'asm1.semantic1.two'" "" { target *-*-* } .-2 }
    {
        int two;
    L2:
        ;
    }
}

void semantic2a(X...)(X expr)
{
    alias X[0] var1;
    asm { "%0" : "=m" (var1); } // { dg-error "cannot modify type 'double'" }
}

void semantic2()
{
   semantic2a(3.6);     // { dg-error "template instance 'asm1.semantic2a!double' error instantiating" }
}

void semantic3()
{
    asm 
    {
        unknown;        // { dg-error "undefined identifier 'unknown'" }
    }
}

struct S4
{
    template opDispatch(string Name, P...)
    {
        static void opDispatch(P) {}
    }
}

void semantic4()
{
    asm
    {
        "%0" : : "m" (S4.foo);  // { dg-error "template instance 'opDispatch!\"foo\"' has no value" }
    }
}
