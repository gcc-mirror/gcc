/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/dep_d1_ops.d(105): Deprecation: `opAdd` is deprecated.  Use `opBinary(string op)(...) if (op == "+")` instead.
fail_compilation/dep_d1_ops.d(106): Deprecation: `opAdd_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "+")` instead.
fail_compilation/dep_d1_ops.d(107): Deprecation: `opSub` is deprecated.  Use `opBinary(string op)(...) if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(108): Deprecation: `opSub_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(109): Deprecation: `opMul` is deprecated.  Use `opBinary(string op)(...) if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(110): Deprecation: `opMul_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(111): Deprecation: `opDiv` is deprecated.  Use `opBinary(string op)(...) if (op == "/")` instead.
fail_compilation/dep_d1_ops.d(112): Deprecation: `opDiv_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "/")` instead.
fail_compilation/dep_d1_ops.d(113): Deprecation: `opMod` is deprecated.  Use `opBinary(string op)(...) if (op == "%")` instead.
fail_compilation/dep_d1_ops.d(114): Deprecation: `opMod_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "%")` instead.
fail_compilation/dep_d1_ops.d(116): Deprecation: `opAnd` is deprecated.  Use `opBinary(string op)(...) if (op == "&")` instead.
fail_compilation/dep_d1_ops.d(117): Deprecation: `opOr` is deprecated.  Use `opBinary(string op)(...) if (op == "|")` instead.
fail_compilation/dep_d1_ops.d(118): Deprecation: `opXor` is deprecated.  Use `opBinary(string op)(...) if (op == "^")` instead.
fail_compilation/dep_d1_ops.d(120): Deprecation: `opShl` is deprecated.  Use `opBinary(string op)(...) if (op == "<<")` instead.
fail_compilation/dep_d1_ops.d(121): Deprecation: `opShl_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "<<")` instead.
fail_compilation/dep_d1_ops.d(122): Deprecation: `opShr` is deprecated.  Use `opBinary(string op)(...) if (op == ">>")` instead.
fail_compilation/dep_d1_ops.d(123): Deprecation: `opShr_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == ">>")` instead.
fail_compilation/dep_d1_ops.d(124): Deprecation: `opUShr` is deprecated.  Use `opBinary(string op)(...) if (op == ">>>")` instead.
fail_compilation/dep_d1_ops.d(125): Deprecation: `opUShr_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == ">>>")` instead.
fail_compilation/dep_d1_ops.d(127): Deprecation: `opCat` is deprecated.  Use `opBinary(string op)(...) if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(128): Deprecation: `opCat_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(130): Deprecation: `opNeg` is deprecated.  Use `opUnary(string op)() if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(131): Deprecation: `opCom` is deprecated.  Use `opUnary(string op)() if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(132): Deprecation: `opPostInc` is deprecated.  Use `opUnary(string op)() if (op == "++")` instead.
fail_compilation/dep_d1_ops.d(133): Deprecation: `opPostDec` is deprecated.  Use `opUnary(string op)() if (op == "--")` instead.
fail_compilation/dep_d1_ops.d(134): Deprecation: `opStar` is deprecated.  Use `opUnary(string op)() if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(136): Deprecation: `opIn` is deprecated.  Use `opBinary(string op)(...) if (op == "in")` instead.
fail_compilation/dep_d1_ops.d(137): Deprecation: `opIn_r` is deprecated.  Use `opBinaryRight(string op)(...) if (op == "in")` instead.
fail_compilation/dep_d1_ops.d(139): Deprecation: `opAddAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "+")` instead.
fail_compilation/dep_d1_ops.d(140): Deprecation: `opSubAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "-")` instead.
fail_compilation/dep_d1_ops.d(141): Deprecation: `opMulAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "*")` instead.
fail_compilation/dep_d1_ops.d(142): Deprecation: `opDivAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "/")` instead.
fail_compilation/dep_d1_ops.d(143): Deprecation: `opModAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "%")` instead.
fail_compilation/dep_d1_ops.d(144): Deprecation: `opAndAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "&")` instead.
fail_compilation/dep_d1_ops.d(145): Deprecation: `opOrAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "|")` instead.
fail_compilation/dep_d1_ops.d(146): Deprecation: `opXorAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "^")` instead.
fail_compilation/dep_d1_ops.d(147): Deprecation: `opShlAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "<<")` instead.
fail_compilation/dep_d1_ops.d(148): Deprecation: `opShrAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == ">>")` instead.
fail_compilation/dep_d1_ops.d(149): Deprecation: `opUShrAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == ">>>")` instead.
fail_compilation/dep_d1_ops.d(150): Deprecation: `opCatAssign` is deprecated.  Use `opOpAssign(string op)(...) if (op == "~")` instead.
fail_compilation/dep_d1_ops.d(158): Deprecation: `opCom` is deprecated.  Use `opUnary(string op)() if (op == "~")` instead.
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
