typedef __bf16 __m128bf16 __attribute__((__vector_size__(16), __aligned__(16)));
typedef __bf16 __m256bf16 __attribute__((__vector_size__(32), __aligned__(32)));
typedef __bf16 __m512bf16 __attribute__((__vector_size__(64), __aligned__(64)));

typedef union
{
  float f;
  unsigned int u;
  __bf16 b[2];
} unionf_b;

static __bf16 make_f32_bf16 (float f)
{
  unionf_b tmp;
  tmp.f = f;
  return tmp.b[1];
}

static float make_bf16_f32 (__bf16 bf)
{
  unionf_b tmp;
  tmp.u = 0;
  tmp.b[1] = bf;
  return tmp.f;
}

static int check_bf16 (__bf16 bf1, __bf16 bf2)
{
  unionf_b tmp1, tmp2;
  tmp1.u = 0;
  tmp2.u = 0;
  tmp1.b[1] = bf1;
  tmp2.b[1] = bf2;
  return (tmp1.u == tmp2.u);
}

static int check_bf16_float (__bf16 bf, float f)
{
  unionf_b tmp1, tmp2;
  tmp1.u = 0;
  tmp1.b[0] = bf;
  tmp2.f = f;
  tmp2.u >>= 16;
  return (tmp1.u == tmp2.u);
}
