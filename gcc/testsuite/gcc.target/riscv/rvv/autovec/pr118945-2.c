/* { dg-do compile } */
/* { dg-options "-march=rva23u64 -mtune=generic-ooo -Ofast -S" } */

void vmult(
    double* dst,
    const double* src,
    const unsigned int* rowstart,
    const unsigned int* colnums,
    const double* val,
    const unsigned int n_rows
) {
    const double* val_ptr = &val[rowstart[0]];
    const unsigned int* colnum_ptr = &colnums[rowstart[0]];
    double* dst_ptr = dst;

    for (unsigned int row = 0; row < n_rows; ++row) {
        double s = 0.;
        const double* const val_end_of_row = &val[rowstart[row + 1]];
        while (val_ptr != val_end_of_row) {
            s += *val_ptr++ * src[*colnum_ptr++];
        }
        *dst_ptr++ = s;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*m[f0-9]+,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*m[f0-9]+,\s*tu,\s*ma} 1 } } */

