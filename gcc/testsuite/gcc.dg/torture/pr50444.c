/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse2" } */
/* { dg-require-effective-target sse2_runtime } */

typedef long long __m128i __attribute__ ((__vector_size__ (16),
__may_alias__));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));
typedef unsigned int uint32_t;

typedef struct {
    uint32_t v[4];
} a4x32;

a4x32* incr(a4x32* x)
{
  x->v[0] += 1;
  return x;
}

typedef struct {
    __m128i m;
} a1xm128i;

static inline  a1xm128i ssefunc( a1xm128i in,  a1xm128i k)
{
  a1xm128i ret;
  ret.m = (__m128i)__builtin_ia32_pxor128 ((__v2di)in.m, (__v2di)k.m);
  return ret;
}

static  a4x32  caster( a4x32 c4x32,  a1xm128i k)
{
  a1xm128i c1x128;
  if( sizeof(c4x32) != sizeof(c1x128) ) __builtin_abort();
  __builtin_memcpy(&c1x128, &c4x32, sizeof(c1x128));
  c1x128 = ssefunc(c1x128, k);
  __builtin_memcpy(&c4x32, &c1x128, sizeof(c4x32));
  return c4x32;
}

typedef struct  {
    a1xm128i key;
    a4x32 c;
    __SIZE_TYPE__ elem;
    a4x32 v;
} Engine;

void ctor(Engine *e)
{
  e->elem = 0;
  e->key.m = (__m128i)(__v4si){ 0, 0, 0, 0 };
  e->c.v[0] = 0;
  e->c.v[1] = 0;
  e->c.v[2] = 0;
  e->c.v[3] = 0;
}

uint32_t method( Engine *e)
{
  if( e->elem == 0 )
    {
      e->v = caster(*incr(&e->c), e->key);
      e->elem = 4;
    }
  return e->v.v[--e->elem];
}

int main()
{
  Engine e4; ctor(&e4);
  Engine e5; ctor(&e5);
  if(method(&e4)!=method(&e5))
    __builtin_abort ();
  return 0;
}
