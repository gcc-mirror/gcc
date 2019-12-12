/*
TEST_OUTPUT:
---
fail_compilation/fail12744.d(38): Error: incompatible parameter storage classes 'ref' and 'out'
fail_compilation/fail12744.d(52): Error: template instance fail12744.bar12744R!(foo12744O) error instantiating
fail_compilation/fail12744.d(38): Error: incompatible parameter storage classes 'ref' and 'lazy'
fail_compilation/fail12744.d(53): Error: template instance fail12744.bar12744R!(foo12744L) error instantiating
fail_compilation/fail12744.d(39): Error: incompatible parameter storage classes 'out' and 'ref'
fail_compilation/fail12744.d(56): Error: template instance fail12744.bar12744O!(foo12744R) error instantiating
fail_compilation/fail12744.d(39): Error: incompatible parameter storage classes 'out' and 'lazy'
fail_compilation/fail12744.d(58): Error: template instance fail12744.bar12744O!(foo12744L) error instantiating
fail_compilation/fail12744.d(40): Error: incompatible parameter storage classes 'lazy' and 'ref'
fail_compilation/fail12744.d(61): Error: template instance fail12744.bar12744L!(foo12744R) error instantiating
fail_compilation/fail12744.d(40): Error: incompatible parameter storage classes 'lazy' and 'out'
fail_compilation/fail12744.d(62): Error: template instance fail12744.bar12744L!(foo12744O) error instantiating
fail_compilation/fail12744.d(41): Error: incompatible parameter storage classes 'auto ref' and 'out'
fail_compilation/fail12744.d(67): Error: template fail12744.bar12744A cannot deduce function from argument types !(foo12744O)(int), candidates are:
fail_compilation/fail12744.d(41):        fail12744.bar12744A(alias f)(auto ref PTT12744!f args)
fail_compilation/fail12744.d(41): Error: incompatible parameter storage classes 'auto ref' and 'lazy'
fail_compilation/fail12744.d(68): Error: template fail12744.bar12744A cannot deduce function from argument types !(foo12744L)(int), candidates are:
fail_compilation/fail12744.d(41):        fail12744.bar12744A(alias f)(auto ref PTT12744!f args)
---
*/
template PTT12744(func...)
{
    static if (is(typeof(func[0]) P == function))
        alias PTT12744 = P;
    else
        static assert(0);
}

void foo12744N(     int x) {}
void foo12744R( ref int x) {}
void foo12744O( out int x) {}
void foo12744L(lazy int x) {}

void bar12744N(alias f)(         PTT12744!f args) {}
void bar12744R(alias f)(     ref PTT12744!f args) {}
void bar12744O(alias f)(     out PTT12744!f args) {}
void bar12744L(alias f)(    lazy PTT12744!f args) {}
void bar12744A(alias f)(auto ref PTT12744!f args) {}

void main()
{
    alias bNN = bar12744N!foo12744N;
    alias bNR = bar12744N!foo12744R;
    alias bNO = bar12744N!foo12744O;
    alias bNL = bar12744N!foo12744L;

    alias bRN = bar12744R!foo12744N;
    alias bRR = bar12744R!foo12744R;
    alias bRO = bar12744R!foo12744O;    // error
    alias bRL = bar12744R!foo12744L;    // error

    alias bON = bar12744O!foo12744N;
    alias bOR = bar12744O!foo12744R;    // error
    alias bOO = bar12744O!foo12744O;
    alias bOL = bar12744O!foo12744L;    // error

    alias bLN = bar12744L!foo12744N;
    alias bLR = bar12744L!foo12744R;    // error
    alias bLO = bar12744L!foo12744O;    // error
    alias bLL = bar12744L!foo12744L;

    bar12744A!foo12744N(1);
    bar12744A!foo12744R(1);
    bar12744A!foo12744O(1);     // error
    bar12744A!foo12744L(1);     // error
}
