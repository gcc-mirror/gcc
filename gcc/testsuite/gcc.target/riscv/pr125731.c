/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcbv_zicond -mabi=lp64d" { target rv64} } */
/* { dg-options "-O2 -march=rv32gcbv_zicond -mabi=ilp32" { target rv32} } */

long fun_not1 (int a, long b)
{
    if (!(a & 1))
	b ^= 8;
    return b;
}

long fun_not2 (short a, int b)
{
    if (!(a & 1))
	b ^= 8;
    return b;
}

long fun_not21 (long a, long b)
{
    if (!(a & 1))
	b ^= 8;
    return b;
}

long fun_not22 (long a, int b)
{
    if (!(a & 1))
	b ^= 8;
    return b;
}

long fun_not3 (int a, long b)
{
    if (!(a & 1))
	b |= 8;
    return b;
}

long fun_not4 (int a, long b)
{
    if (!(a & 1))
	b &= 8;
    return b;
}

/* { dg-final { scan-assembler-not "\tbne" } } */
