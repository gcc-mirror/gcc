/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */

typedef int v4si __attribute__ ((__vector_size__ (16)));

v4si i00(v4si x) { return (v4si){x[0],0,0,0}; }
v4si i10(v4si x) { return (v4si){x[1],0,0,0}; }
v4si i20(v4si x) { return (v4si){x[2],0,0,0}; }
v4si i30(v4si x) { return (v4si){x[3],0,0,0}; }

v4si i01(v4si x) { return (v4si){0,x[0],0,0}; }
v4si i11(v4si x) { return (v4si){0,x[1],0,0}; }
v4si i21(v4si x) { return (v4si){0,x[2],0,0}; }
v4si i31(v4si x) { return (v4si){0,x[3],0,0}; }

v4si i02(v4si x) { return (v4si){0,0,x[0],0}; }
v4si i12(v4si x) { return (v4si){0,0,x[1],0}; }
v4si i22(v4si x) { return (v4si){0,0,x[2],0}; }
v4si i32(v4si x) { return (v4si){0,0,x[3],0}; }

v4si i03(v4si x) { return (v4si){0,0,0,x[0]}; }
v4si i13(v4si x) { return (v4si){0,0,0,x[1]}; }
v4si i23(v4si x) { return (v4si){0,0,0,x[2]}; }
v4si i33(v4si x) { return (v4si){0,0,0,x[3]}; }

typedef v4si (*fun_t)(v4si);
typedef struct {
  fun_t fun;
  int a0, a1, a2, a3;
  int b0, b1, b2, b3;
} test_t;

const test_t tests[16] = {
  { i00,  1, 0, 0, 0,  4, 0, 0, 0 },
  { i10,  2, 0, 0, 0,  3, 0, 0, 0 },
  { i20,  3, 0, 0, 0,  2, 0, 0, 0 },
  { i30,  4, 0, 0, 0,  1, 0, 0, 0 },
  { i01,  0, 1, 0, 0,  0, 4, 0, 0 },
  { i11,  0, 2, 0, 0,  0, 3, 0, 0 },
  { i21,  0, 3, 0, 0,  0, 2, 0, 0 },
  { i31,  0, 4, 0, 0,  0, 1, 0, 0 },
  { i02,  0, 0, 1, 0,  0, 0, 4, 0 },
  { i12,  0, 0, 2, 0,  0, 0, 3, 0 },
  { i22,  0, 0, 3, 0,  0, 0, 2, 0 },
  { i32,  0, 0, 4, 0,  0, 0, 1, 0 },
  { i03,  0, 0, 0, 1,  0, 0, 0, 4 },
  { i13,  0, 0, 0, 2,  0, 0, 0, 3 },
  { i23,  0, 0, 0, 3,  0, 0, 0, 2 },
  { i33,  0, 0, 0, 4,  0, 0, 0, 1 }
};

int main()
{
#ifdef CHECK_ISA
  check_isa ();
#endif

  int i;
  v4si a = (v4si){ 1, 2, 3, 4 };
  v4si b = (v4si){ 4, 3, 2, 1 };
  for (i = 0; i < 16; i++) {
  const test_t *p = &tests[i];
    v4si o1 = (*p->fun)(a);
    if (o1[0] != p->a0 || o1[1] != p->a1 || o1[2] != p->a2 || o1[3] != p->a3)
      __builtin_abort ();

    v4si o2 = (*p->fun)(b);
    if (o2[0] != p->b0 || o2[1] != p->b1 || o2[2] != p->b2 || o2[3] != p->b3)
      __builtin_abort ();
  }
  return 0;
}
