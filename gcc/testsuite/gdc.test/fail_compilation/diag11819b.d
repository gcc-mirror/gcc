/*
TEST_OUTPUT:
---
fail_compilation/diag11819b.d(28): Error: unrecognized trait `HasMember`, did you mean `hasMember`?
fail_compilation/diag11819b.d(29): Error: unrecognized trait `Identifier`, did you mean `identifier`?
fail_compilation/diag11819b.d(30): Error: unrecognized trait `GetProtection`, did you mean `getProtection`?
fail_compilation/diag11819b.d(31): Error: unrecognized trait `Parent`, did you mean `parent`?
fail_compilation/diag11819b.d(32): Error: unrecognized trait `GetMember`, did you mean `getMember`?
fail_compilation/diag11819b.d(33): Error: unrecognized trait `GetOverloads`, did you mean `getOverloads`?
fail_compilation/diag11819b.d(34): Error: unrecognized trait `GetVirtualFunctions`, did you mean `getVirtualFunctions`?
fail_compilation/diag11819b.d(35): Error: unrecognized trait `GetVirtualMethods`, did you mean `getVirtualMethods`?
fail_compilation/diag11819b.d(36): Error: unrecognized trait `ClassInstanceSize`, did you mean `classInstanceSize`?
fail_compilation/diag11819b.d(37): Error: unrecognized trait `AllMembers`, did you mean `allMembers`?
fail_compilation/diag11819b.d(38): Error: unrecognized trait `DerivedMembers`, did you mean `derivedMembers`?
fail_compilation/diag11819b.d(39): Error: unrecognized trait `IsSame`, did you mean `isSame`?
fail_compilation/diag11819b.d(40): Error: unrecognized trait `Compiles`, did you mean `compiles`?
fail_compilation/diag11819b.d(41): Error: unrecognized trait `Parameters`
fail_compilation/diag11819b.d(42): Error: unrecognized trait `GetAliasThis`, did you mean `getAliasThis`?
fail_compilation/diag11819b.d(43): Error: unrecognized trait `GetAttributes`, did you mean `getAttributes`?
fail_compilation/diag11819b.d(44): Error: unrecognized trait `GetFunctionAttributes`, did you mean `getFunctionAttributes`?
fail_compilation/diag11819b.d(45): Error: unrecognized trait `GetUnitTests`, did you mean `getUnitTests`?
fail_compilation/diag11819b.d(46): Error: unrecognized trait `GetVirtualIndex`, did you mean `getVirtualIndex`?
---
*/

void main()
{
    if (__traits(HasMember)) { }
    if (__traits(Identifier)) { }
    if (__traits(GetProtection)) { }
    if (__traits(Parent)) { }
    if (__traits(GetMember)) { }
    if (__traits(GetOverloads)) { }
    if (__traits(GetVirtualFunctions)) { }
    if (__traits(GetVirtualMethods)) { }
    if (__traits(ClassInstanceSize)) { }
    if (__traits(AllMembers)) { }
    if (__traits(DerivedMembers)) { }
    if (__traits(IsSame)) { }
    if (__traits(Compiles)) { }
    if (__traits(Parameters)) { }
    if (__traits(GetAliasThis)) { }
    if (__traits(GetAttributes)) { }
    if (__traits(GetFunctionAttributes)) { }
    if (__traits(GetUnitTests)) { }
    if (__traits(GetVirtualIndex)) { }
}
