/*
TEST_OUTPUT:
---
fail_compilation/diag11819a.d(30): Error: unrecognized trait `DoesNotExist`
fail_compilation/diag11819a.d(31): Error: unrecognized trait `IsAbstractClass`, did you mean `isAbstractClass`?
fail_compilation/diag11819a.d(32): Error: unrecognized trait `IsArithmetic`, did you mean `isArithmetic`?
fail_compilation/diag11819a.d(33): Error: unrecognized trait `IsAssociativeArray`, did you mean `isAssociativeArray`?
fail_compilation/diag11819a.d(34): Error: unrecognized trait `IsFinalClass`, did you mean `isFinalClass`?
fail_compilation/diag11819a.d(35): Error: unrecognized trait `IsPOD`, did you mean `isPOD`?
fail_compilation/diag11819a.d(36): Error: unrecognized trait `IsNested`, did you mean `isNested`?
fail_compilation/diag11819a.d(37): Error: unrecognized trait `IsFloating`, did you mean `isFloating`?
fail_compilation/diag11819a.d(38): Error: unrecognized trait `IsIntegral`, did you mean `isIntegral`?
fail_compilation/diag11819a.d(39): Error: unrecognized trait `IsScalar`, did you mean `isScalar`?
fail_compilation/diag11819a.d(40): Error: unrecognized trait `IsStaticArray`, did you mean `isStaticArray`?
fail_compilation/diag11819a.d(41): Error: unrecognized trait `IsUnsigned`, did you mean `isUnsigned`?
fail_compilation/diag11819a.d(42): Error: unrecognized trait `IsVirtualFunction`, did you mean `isVirtualFunction`?
fail_compilation/diag11819a.d(43): Error: unrecognized trait `IsVirtualMethod`, did you mean `isVirtualMethod`?
fail_compilation/diag11819a.d(44): Error: unrecognized trait `IsAbstractFunction`, did you mean `isAbstractFunction`?
fail_compilation/diag11819a.d(45): Error: unrecognized trait `IsFinalFunction`, did you mean `isFinalFunction`?
fail_compilation/diag11819a.d(46): Error: unrecognized trait `IsOverrideFunction`, did you mean `isOverrideFunction`?
fail_compilation/diag11819a.d(47): Error: unrecognized trait `IsStaticFunction`, did you mean `isStaticFunction`?
fail_compilation/diag11819a.d(48): Error: unrecognized trait `IsRef`, did you mean `isRef`?
fail_compilation/diag11819a.d(49): Error: unrecognized trait `IsOut`, did you mean `isOut`?
fail_compilation/diag11819a.d(50): Error: unrecognized trait `IsLazy`, did you mean `isLazy`?
---
*/

void main()
{
    if (__traits(DoesNotExist)) { }
    if (__traits(IsAbstractClass)) { }
    if (__traits(IsArithmetic)) { }
    if (__traits(IsAssociativeArray)) { }
    if (__traits(IsFinalClass)) { }
    if (__traits(IsPOD)) { }
    if (__traits(IsNested)) { }
    if (__traits(IsFloating)) { }
    if (__traits(IsIntegral)) { }
    if (__traits(IsScalar)) { }
    if (__traits(IsStaticArray)) { }
    if (__traits(IsUnsigned)) { }
    if (__traits(IsVirtualFunction)) { }
    if (__traits(IsVirtualMethod)) { }
    if (__traits(IsAbstractFunction)) { }
    if (__traits(IsFinalFunction)) { }
    if (__traits(IsOverrideFunction)) { }
    if (__traits(IsStaticFunction)) { }
    if (__traits(IsRef)) { }
    if (__traits(IsOut)) { }
    if (__traits(IsLazy)) { }
}
