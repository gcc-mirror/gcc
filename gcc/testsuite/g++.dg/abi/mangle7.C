/* { dg-do compile } */

typedef void *const t1[2];
float const f1(t1 (&)[79], ...) { return 0.0f; }

/* { dg-final { scan-assembler _Z2f1RA79_A2_KPvz } } */
