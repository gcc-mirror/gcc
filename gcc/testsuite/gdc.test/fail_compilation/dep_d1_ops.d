/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/dep_d1_ops.d(105): Error: `opAdd` is obsolete.  Use `opBinary(string op)(...) if (op == "+")` instead.
fail_compilation/dep_d1_ops.d(106): Error: `opAdd_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "+")` instead.
fail_compilation/dep_d1_ops.d(107): Error: `opSub` is obsolete.  Use `opBinary(string op)(...) if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(108): Error: `opSub_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(109): Error: `opMul` is obsolete.  Use `opBinary(string op)(...) if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(110): Error: `opMul_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(111): Error: `opDiv` is obsolete.  Use `opBinary(string op)(...) if (op == "/")` instead.
fail_compilation/dep_d1_ops.d(112): Error: `opDiv_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "/")` instead.
fail_compilation/dep_d1_ops.d(113): Error: `opMod` is obsolete.  Use `opBinary(string op)(...) if (op == "%")` instead.
fail_compilation/dep_d1_ops.d(114): Error: `opMod_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "%")` instead.
fail_compilation/dep_d1_ops.d(116): Error: `opAnd` is obsolete.  Use `opBinary(string op)(...) if (op == "&")` instead.
fail_compilation/dep_d1_ops.d(117): Error: `opOr` is obsolete.  Use `opBinary(string op)(...) if (op == "|")` instead.
fail_compilation/dep_d1_ops.d(118): Error: `opXor` is obsolete.  Use `opBinary(string op)(...) if (op == "^")` instead.
fail_compilation/dep_d1_ops.d(120): Error: `opShl` is obsolete.  Use `opBinary(string op)(...) if (op == "<<")` instead.
fail_compilation/dep_d1_ops.d(121): Error: `opShl_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "<<")` instead.
fail_compilation/dep_d1_ops.d(122): Error: `opShr` is obsolete.  Use `opBinary(string op)(...) if (op == ">>")` instead.
fail_compilation/dep_d1_ops.d(123): Error: `opShr_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == ">>")` instead.
fail_compilation/dep_d1_ops.d(124): Error: `opUShr` is obsolete.  Use `opBinary(string op)(...) if (op == ">>>")` instead.
fail_compilation/dep_d1_ops.d(125): Error: `opUShr_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == ">>>")` instead.
fail_compilation/dep_d1_ops.d(127): Error: `opCat` is obsolete.  Use `opBinary(string op)(...) if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(128): Error: `opCat_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(130): Error: `opNeg` is obsolete.  Use `opUnary(string op)() if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(131): Error: `opCom` is obsolete.  Use `opUnary(string op)() if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(132): Error: `opPostInc` is obsolete.  Use `opUnary(string op)() if (op == "++")` instead.
fail_compilation/dep_d1_ops.d(133): Error: `opPostDec` is obsolete.  Use `opUnary(string op)() if (op == "--")` instead.
fail_compilation/dep_d1_ops.d(134): Error: `opStar` is obsolete.  Use `opUnary(string op)() if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(136): Error: `opIn` is obsolete.  Use `opBinary(string op)(...) if (op == "in")` instead.
fail_compilation/dep_d1_ops.d(137): Error: `opIn_r` is obsolete.  Use `opBinaryRight(string op)(...) if (op == "in")` instead.
fail_compilation/dep_d1_ops.d(139): Error: `opAddAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "+")` instead.
fail_compilation/dep_d1_ops.d(140): Error: `opSubAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(141): Error: `opMulAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(142): Error: `opDivAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "/")` instead.
fail_compilation/dep_d1_ops.d(143): Error: `opModAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "%")` instead.
fail_compilation/dep_d1_ops.d(144): Error: `opAndAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "&")` instead.
fail_compilation/dep_d1_ops.d(145): Error: `opOrAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "|")` instead.
fail_compilation/dep_d1_ops.d(146): Error: `opXorAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "^")` instead.
fail_compilation/dep_d1_ops.d(147): Error: `opShlAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "<<")` instead.
fail_compilation/dep_d1_ops.d(148): Error: `opShrAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == ">>")` instead.
fail_compilation/dep_d1_ops.d(149): Error: `opUShrAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == ">>>")` instead.
fail_compilation/dep_d1_ops.d(150): Error: `opCatAssign` is obsolete.  Use `opOpAssign(string op)(...) if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(158): Error: `opCom` is obsolete.  Use `opUnary(string op)() if (op == "~")` instead.
---
*/

#line 50
struct S
{
    int opAdd(int i) { return 0; }
    int opAdd_r(int i) { return 0; }
    int opSub(int i) { return 0; }
    int opSub_r(int i) { return 0; }
    int opMul(int i) { return 0; }
    int opMul_r(int i) { return 0; }
    int opDiv(int i) { return 0; }
    int opDiv_r(int i) { return 0; }
    int opMod(int i) { return 0; }
    int opMod_r(int i) { return 0; }

    int opAnd(int i) { return 0; }
    int opOr(int i) { return 0; }
    int opXor(int i) { return 0; }

    int opShl(int i) { return 0; }
    int opShl_r(int i) { return 0; }
    int opShr(int i) { return 0; }
    int opShr_r(int i) { return 0; }
    int opUShr(int i) { return 0; }
    int opUShr_r(int i) { return 0; }

    int opCat(int i) { return 0; }
    int opCat_r(int i) { return 0; }

    int opNeg() { return 0; }
    int opCom() { return 0; }
    int opPostInc() { return 0; }
    int opPostDec() { return 0; }
    int opStar() { return 0; }

    int opIn(int i) { return 0; }
    int opIn_r(int i) { return 0; }

    int opAddAssign(int i) { return 0; }
    int opSubAssign(int i) { return 0; }
    int opMulAssign(int i) { return 0; }
    int opDivAssign(int i) { return 0; }
    int opModAssign(int i) { return 0; }
    int opAndAssign(int i) { return 0; }
    int opOrAssign(int i) { return 0; }
    int opXorAssign(int i) { return 0; }
    int opShlAssign(int i) { return 0; }
    int opShrAssign(int i) { return 0; }
    int opUShrAssign(int i) { return 0; }
    int opCatAssign(int i) { return 0; }
}

void main()
{
    S s;
    int i;

    i = s + 1;
    i = 1 + s;
    i = s - 1;
    i = 1 - s;
    i = s * 1;
    i = 1 * s;
    i = s / 1;
    i = 1 / s;
    i = s % 1;
    i = 1 % s;

    i = s & 1;
    i = s | 1;
    i = s ^ 1;

    i = s << 1;
    i = 1 << s;
    i = s >> 1;
    i = 1 >> s;
    i = s >>> 1;
    i = 1 >>> s;

    i = s ~ 1;
    i = 1 ~ s;

    i = -s;
    i = ~s;
    s++;
    s--;
    i = *s;

    i = s in 1;
    i = 1 in s;

    s += 1;
    s -= 1;
    s *= 1;
    s /= 1;
    s %= 1;
    s &= 1;
    s |= 1;
    s ^= 1;
    s <<= 1;
    s >>= 1;
    s >>>= 1;
    s ~= 1;

    scope nd = new NoDeprecation;
    assert((42 in nd) == 0);
    assert((nd in 42) == 0);
    assert((nd ~ 42) == 0);
    assert((42 ~ nd) == 0);

    ~nd;
}

/// See https://github.com/dlang/dmd/pull/10716
class NoDeprecation
{
    int opIn(int i) { return 0; }
    int opIn_r(int i) { return 0; }
    int opCat(int i) { return 0; }
    int opCat_r(int i) { return 0; }

    /// This is considered because there is no `opUnary`
    /// However, the other overloads (`opBinary` / `opBinaryRight`)
    /// means that other operator overloads would not be considered.
    int opCom() { return 0; }

    int opBinary(string op)(int arg)
        if (op == "in" || op == "~")
    {
        static if (op == "in")
            return this.opIn(arg);
        else static if (op == "~")
            return this.opCat(arg);
    }

    int opBinaryRight(string op)(int arg)
        if (op == "in" || op == "~")
    {
        static if (op == "in")
            return this.opIn_r(arg);
        else static if (op == "~")
            return this.opCat_r(arg);
    }
}
