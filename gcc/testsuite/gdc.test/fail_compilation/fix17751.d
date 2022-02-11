/* REQUIRED_ARGS: -m64
 * TEST_OUTPUT:
---
fail_compilation/fix17751.d(15): Error: last parameter to `__simd()` must be a constant
---
 */

// https://issues.dlang.org/show_bug.cgi?id=17751

import core.simd;

pure @safe V1 simd(XMM opcode, V1, V2)(V1 op1, V2 op2, ubyte imm8)
    if (is(V1 == __vector) && is(V2 == __vector))
{
    return cast(V1)__simd(opcode, op1, op2, imm8);
}

void main()
{
    float4 a, b;
    a = simd!(XMM.CMPPD)(a, b, 0x7A);
}
