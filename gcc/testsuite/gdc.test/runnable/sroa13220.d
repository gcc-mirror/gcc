/* REQUIRED_ARGS: -O -inline -noboundscheck
 */
// https://github.com/dlang/pull/13220

version (D_SIMD)
{

mixin template VectorOps(VectorType, ArrayType: BaseType[N], BaseType, size_t N)
{
    enum Count = N;
    alias Base = BaseType;

    BaseType* ptr() return pure nothrow @nogc
    {
        return array.ptr;
    }

    // Unary operators
    VectorType opUnary(string op)() pure nothrow @safe @nogc
    {
        VectorType res = void;
        mixin("res.array[] = " ~ op ~ "array[];");
        return res;
    }

    // Binary operators
    VectorType opBinary(string op)(VectorType other) pure const nothrow @safe @nogc
    {
        VectorType res = void;
        mixin("res.array[] = array[] " ~ op ~ " other.array[];");
        return res;
    }

    // Assigning a BaseType value
    void opAssign(BaseType e) pure nothrow @safe @nogc
    {
        array[] = e;
    }

    // Assigning a static array
    void opAssign(ArrayType v) pure nothrow @safe @nogc
    {
        array[] = v[];
    }

    void opOpAssign(string op)(VectorType other) pure nothrow @safe @nogc
    {
        mixin("array[] "  ~ op ~ "= other.array[];");
    }

    // Assigning a dyn array
    this(ArrayType v) pure nothrow @safe @nogc
    {
        array[] = v[];
    }

    // Broadcast constructor
    this(BaseType x) pure nothrow @safe @nogc
    {
        array[] = x;
    }

    ref inout(BaseType) opIndex(size_t i) inout pure nothrow @safe @nogc
    {
        return array[i];
    }
}

// Note: can't be @safe with this signature
Vec loadUnaligned(Vec)(const(BaseType!Vec)* pvec) @trusted
{
    // Since this vector is emulated, it doesn't have alignement constraints
    // and as such we can just cast it.
    return *cast(Vec*)(pvec);
}

private template BaseType(V)
{
    alias typeof( ( { V v; return v; }()).array[0]) BaseType;
}

struct int4
{
    int[4] array;
    mixin VectorOps!(int4, int[4]);
}

alias __m128i = int4;
}

int main()
{
  version (D_SIMD)
  {
    int4 A = [1, 2, 3, 4];
    int4 ia = A;
    ia.ptr[2] = 5;
    int4 C = ia;
    int[4] result = [1, 2, 5, 4];
    assert(C.array == result);
  }
    return 0;
}
