/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -Ofast" } */

float g(float q[], int N){
    float dqnorm = 0.0;

    #pragma GCC unroll 4

    for (int i=0; i < N; i++) {
        dqnorm = dqnorm + q[i] * q[i];
    }
    return dqnorm;
}

/* need slightly different test for when -funroll-loops is enabled to keep
   test output stable.  Otherwise test may be flakey.  */
/* { dg-final { scan-assembler-times {beq\s+[a-x0-9]+,zero,.L12\s+vsetvli} 3 { target { no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {beq\s+[a-x0-9]+,[a-x0-9]+,.L12\s+vsetvli} 3 { target { any-opts "-funroll-loops" } } } } */

