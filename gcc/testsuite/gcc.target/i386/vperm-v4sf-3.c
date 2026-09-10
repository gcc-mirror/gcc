/* { dg-do run } */
/* { dg-options "-O2" } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

v4sf f00(v4sf x) { return (v4sf){x[0],0.0f,0.0f,0.0f}; }
v4sf f10(v4sf x) { return (v4sf){x[1],0.0f,0.0f,0.0f}; }
v4sf f20(v4sf x) { return (v4sf){x[2],0.0f,0.0f,0.0f}; }
v4sf f30(v4sf x) { return (v4sf){x[3],0.0f,0.0f,0.0f}; }

v4sf f01(v4sf x) { return (v4sf){0.0f,x[0],0.0f,0.0f}; }
v4sf f11(v4sf x) { return (v4sf){0.0f,x[1],0.0f,0.0f}; }
v4sf f21(v4sf x) { return (v4sf){0.0f,x[2],0.0f,0.0f}; }
v4sf f31(v4sf x) { return (v4sf){0.0f,x[3],0.0f,0.0f}; }

v4sf f02(v4sf x) { return (v4sf){0.0f,0.0f,x[0],0.0f}; }
v4sf f12(v4sf x) { return (v4sf){0.0f,0.0f,x[1],0.0f}; }
v4sf f22(v4sf x) { return (v4sf){0.0f,0.0f,x[2],0.0f}; }
v4sf f32(v4sf x) { return (v4sf){0.0f,0.0f,x[3],0.0f}; }

v4sf f03(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[0]}; }
v4sf f13(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[1]}; }
v4sf f23(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[2]}; }
v4sf f33(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[3]}; }


typedef v4sf (*fun_t)(v4sf);
typedef struct {
  fun_t fun;
  float a0, a1, a2, a3;
  float b0, b1, b2, b3;
} test_t;

const test_t tests[16] = {
  { f00,  1.0f, 0.0f, 0.0f, 0.0f,  4.0f, 0.0f, 0.0f, 0.0f },
  { f10,  2.0f, 0.0f, 0.0f, 0.0f,  3.0f, 0.0f, 0.0f, 0.0f },
  { f20,  3.0f, 0.0f, 0.0f, 0.0f,  2.0f, 0.0f, 0.0f, 0.0f },
  { f30,  4.0f, 0.0f, 0.0f, 0.0f,  1.0f, 0.0f, 0.0f, 0.0f },
  { f01,  0.0f, 1.0f, 0.0f, 0.0f,  0.0f, 4.0f, 0.0f, 0.0f },
  { f11,  0.0f, 2.0f, 0.0f, 0.0f,  0.0f, 3.0f, 0.0f, 0.0f },
  { f21,  0.0f, 3.0f, 0.0f, 0.0f,  0.0f, 2.0f, 0.0f, 0.0f },
  { f31,  0.0f, 4.0f, 0.0f, 0.0f,  0.0f, 1.0f, 0.0f, 0.0f },
  { f02,  0.0f, 0.0f, 1.0f, 0.0f,  0.0f, 0.0f, 4.0f, 0.0f },
  { f12,  0.0f, 0.0f, 2.0f, 0.0f,  0.0f, 0.0f, 3.0f, 0.0f },
  { f22,  0.0f, 0.0f, 3.0f, 0.0f,  0.0f, 0.0f, 2.0f, 0.0f },
  { f32,  0.0f, 0.0f, 4.0f, 0.0f,  0.0f, 0.0f, 1.0f, 0.0f },
  { f03,  0.0f, 0.0f, 0.0f, 1.0f,  0.0f, 0.0f, 0.0f, 4.0f },
  { f13,  0.0f, 0.0f, 0.0f, 2.0f,  0.0f, 0.0f, 0.0f, 3.0f },
  { f23,  0.0f, 0.0f, 0.0f, 3.0f,  0.0f, 0.0f, 0.0f, 2.0f },
  { f33,  0.0f, 0.0f, 0.0f, 4.0f,  0.0f, 0.0f, 0.0f, 1.0f }
};

int main()
{
#ifdef CHECK_ISA
  check_isa ();
#endif

  int i;
  v4sf a = (v4sf){ 1.0f, 2.0f, 3.0f, 4.0f };
  v4sf b = (v4sf){ 4.0f, 3.0f, 2.0f, 1.0f };
  for (i = 0; i < 16; i++) {
  const test_t *p = &tests[i];
    v4sf o1 = (*p->fun)(a);
    if (o1[0] != p->a0 || o1[1] != p->a1 || o1[2] != p->a2 || o1[3] != p->a3)
      __builtin_abort ();

    v4sf o2 = (*p->fun)(b);
    if (o2[0] != p->b0 || o2[1] != p->b1 || o2[2] != p->b2 || o2[3] != p->b3)
      __builtin_abort ();
  }
  return 0;
}

