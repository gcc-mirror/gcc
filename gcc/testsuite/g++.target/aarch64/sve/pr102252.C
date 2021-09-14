/* PR target/102252.  */
/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-march=armv8.2-a+sve -msve-vector-bits=512" } */

/* We used to generate invalid assembly for SVE predicate loads.  */

#include <arm_sve.h>

class SimdBool
{
private:
    typedef svbool_t simdInternalType_ __attribute__((arm_sve_vector_bits(512)));

public:
    SimdBool() {}

    simdInternalType_ simdInternal_;

};

static svfloat32_t selectByMask(svfloat32_t a, SimdBool m) {
    return svsel_f32(m.simdInternal_, a, svdup_f32(0.0));
}

struct s {
    SimdBool array[1];
};



void foo(struct s* const work, int offset)
{
        svfloat32_t tz_S0;

        tz_S0 = selectByMask(tz_S0, work->array[offset]);
}

