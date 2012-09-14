/* { dg-do compile } */
/* { dg-prune-output "in evaluation of" } */
#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

int main (int argc, char *argv[]) {
    vector(4, float) vfloat0 = {1., 2., 3., 4.};
    vector(4, float) vfloat1 = {1., 2., 3., 4.};
    
    vector(4,   int) vint   = {1,  1,  1,  1 };
    
    vint <<= vfloat0;  /* { dg-error "nvalid operands to binary <<" } */
    vfloat0 >>= vint;  /* { dg-error "nvalid operands to binary >>" } */

    vfloat0 <<= vfloat1;  /* { dg-error "nvalid operands" } */

    return 0;
}

