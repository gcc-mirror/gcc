/* REQUIRED_ARGS: -O -release -inline
This compares two different ways to do a for loop. The range
version should SROA the VecRange struct into two register variables.
*/

extern (C):

nothrow:
@nogc:
@safe:

alias vec_base_t = size_t;                     // base type of vector
alias vec_t = vec_base_t*;

@trusted
pure
size_t vec_index(size_t b, const vec_t vec);

@trusted
pure ref inout(vec_base_t) vec_numbits(inout vec_t v) { return v[-1]; }
@trusted
pure ref inout(vec_base_t) vec_dim(inout vec_t v) { return v[-2]; }

struct VecRange
{
    size_t i;
    const vec_t v;

  @nogc @safe nothrow pure:
    this(const vec_t v) { this.v = v; i = vec_index(0, v); }
    bool empty() const { return i == vec_numbits(v); }
    size_t front() const { return i; }
    void popFront() { i = vec_index(i + 1, v); }
}

@safe
pure
uint vec_numBitsSet(const vec_t vec)
{
    uint n = 0;
    size_t length = vec_numbits(vec);
    for (size_t i = 0; (i = vec_index(i, vec)) < length; ++i)
        ++n;
    return n;
}

@safe
pure
uint vec_numBitsSet2(const vec_t vec)
{
    uint n = 0;
    foreach (j; VecRange(vec))
        ++n;
    return n;
}
