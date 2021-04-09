/* TEST_OUTPUT:
---
fail_compilation/test17096.d(28): Error: expected 1 arguments for `isPOD` but had 2
fail_compilation/test17096.d(29): Error: expected 1 arguments for `isNested` but had 2
fail_compilation/test17096.d(30): Error: expected 1 arguments for `isVirtualFunction` but had 2
fail_compilation/test17096.d(31): Error: expected 1 arguments for `isVirtualMethod` but had 2
fail_compilation/test17096.d(32): Error: expected 1 arguments for `isAbstractFunction` but had 2
fail_compilation/test17096.d(33): Error: expected 1 arguments for `isFinalFunction` but had 2
fail_compilation/test17096.d(34): Error: expected 1 arguments for `isOverrideFunction` but had 2
fail_compilation/test17096.d(35): Error: expected 1 arguments for `isStaticFunction` but had 2
fail_compilation/test17096.d(36): Error: expected 1 arguments for `isRef` but had 2
fail_compilation/test17096.d(37): Error: expected 1 arguments for `isOut` but had 2
fail_compilation/test17096.d(38): Error: expected 1 arguments for `isLazy` but had 2
fail_compilation/test17096.d(39): Error: expected 1 arguments for `identifier` but had 2
fail_compilation/test17096.d(40): Error: expected 1 arguments for `getProtection` but had 2
fail_compilation/test17096.d(41): Error: expected 1 arguments for `parent` but had 2
fail_compilation/test17096.d(42): Error: expected 1 arguments for `classInstanceSize` but had 2
fail_compilation/test17096.d(43): Error: expected 1 arguments for `allMembers` but had 2
fail_compilation/test17096.d(44): Error: expected 1 arguments for `derivedMembers` but had 2
fail_compilation/test17096.d(45): Error: expected 1 arguments for `getAliasThis` but had 2
fail_compilation/test17096.d(46): Error: expected 1 arguments for `getAttributes` but had 2
fail_compilation/test17096.d(47): Error: expected 1 arguments for `getFunctionAttributes` but had 2
fail_compilation/test17096.d(48): Error: expected 1 arguments for `getUnitTests` but had 2
fail_compilation/test17096.d(49): Error: expected 1 arguments for `getVirtualIndex` but had 2
fail_compilation/test17096.d(50): Error: a single type expected for trait pointerBitmap
---
*/
enum b03 = __traits(isPOD, 1, 2);
enum b04 = __traits(isNested, 1, 2);
enum b05 = __traits(isVirtualFunction, 1, 2);
enum b06 = __traits(isVirtualMethod, 1, 2);
enum b07 = __traits(isAbstractFunction, 1, 2);
enum b08 = __traits(isFinalFunction, 1, 2);
enum b09 = __traits(isOverrideFunction, 1, 2);
enum b10 = __traits(isStaticFunction, 1, 2);
enum b11 = __traits(isRef, 1, 2);
enum b12 = __traits(isOut, 1, 2);
enum b13 = __traits(isLazy, 1, 2);
enum b14 = __traits(identifier, 1, 2);
enum b15 = __traits(getProtection, 1, 2);
enum b16 = __traits(parent, 1, 2);
enum b17 = __traits(classInstanceSize, 1, 2);
enum b18 = __traits(allMembers, 1, 2);
enum b19 = __traits(derivedMembers, 1, 2);
enum b20 = __traits(getAliasThis, 1, 2);
enum b21 = __traits(getAttributes, 1, 2);
enum b22 = __traits(getFunctionAttributes, 1, 2);
enum b23 = __traits(getUnitTests, 1, 2);
enum b24 = __traits(getVirtualIndex, 1, 2);
enum b25 = __traits(getPointerBitmap, 1, 2);
