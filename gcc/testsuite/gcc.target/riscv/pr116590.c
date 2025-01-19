/* { dg-do assemble } */
/* { dg-options "-march=rv64gc_zfh_xtheadvector -mabi=lp64d" } */
typedef long unsigned int size_t;
#pragma riscv intrinsic "vector"


void vmv8r()
{
    float x[32];

    size_t vl = __riscv_vsetvl_e32m8(32);
    vfloat32m8_t _p = __riscv_vle32_v_f32m8(x, vl);

    _p = __riscv_vfmacc_vf_f32m8(__riscv_vfmv_v_f_f32m8(0.5f, vl), 1.442, _p, vl);

    __riscv_vse32_v_f32m8(x, _p, vl);
}

void gen_vmv8r(float* ptr, int n)
{
    while (n > 0)
    {
        size_t vl = __riscv_vsetvl_e32m8(n);

        vfloat32m8_t _p = __riscv_vle32_v_f32m8(ptr, vl);

        _p = __riscv_vfmacc_vf_f32m8(__riscv_vfmv_v_f_f32m8(0.5f, vl), 1.4f, _p, vl);

        __riscv_vse32_v_f32m8(ptr, _p, vl);

        ptr += vl;
        n -= vl;
    }
}

void no_vmv8r(float* ptr, int n)
{
    size_t vl0 = __riscv_vsetvl_e32m8(n);
    vfloat32m8_t _a = __riscv_vfmv_v_f_f32m8(0.5f, vl0);

    while (n > 0)
    {
        size_t vl = __riscv_vsetvl_e32m8(n);

        vfloat32m8_t _p = __riscv_vle32_v_f32m8(ptr, vl);

        _p = __riscv_vfmacc_vf_f32m8(_a, 1.4f, _p, vl);

        __riscv_vse32_v_f32m8(ptr, _p, vl);

        ptr += vl;
        n -= vl;
    }
}
