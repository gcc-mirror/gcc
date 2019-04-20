/* PR c/89685 - ICE on attribute copy with a compound expression
   { dg-do compile }
   { dg-options "-Wall -Wno-unused-value -Wno-int-to-pointer-cast" } */

#if __cplusplus <= 199711L
#  define static_assert(expr) typedef char Assert [1 - !(expr) * 2]
#endif

#define ATTR(...) __attribute__ ((__VA_ARGS__))

typedef struct ATTR (packed) A { ATTR (packed) unsigned bf: 1; } A;

typedef struct B
{
  struct A a;
  struct A *pa;
  struct A &ra;
} B;

extern struct A a;
extern struct A *pa;
extern struct A &ra;
extern B b;
extern B ab[1];
extern B *pb;
extern B &rb;

typedef struct C
{
  ATTR (copy ((struct A *)0)) short m_pa_0;
  ATTR (copy ((struct A *)(1, 0))) int m_pa_1_0;
  ATTR (copy ((struct A *)(0, 1))) long m_pa_0_1;

  ATTR (copy (*(struct A *)0)) short m_xpa_0;
  ATTR (copy (*(struct A *)(1, 0))) int m_xpa_1_0;
  ATTR (copy (*(struct A *)(0, 1))) long m_xpa_0_1;

  ATTR (copy (((struct A *)0)[0])) short m_arpa_0;
  ATTR (copy (((struct A *)(1, 0))[0])) int m_arpa_1_0;
  ATTR (copy (((struct A *)(0, 1))[0])) long m_arpa_0_1;

  ATTR (copy (a)) short m_a;
  ATTR (copy (b.a)) int m_b_a;
  ATTR (copy (b.pa)) long m_b_pa;
  ATTR (copy (b.ra)) long m_b_ra;

  ATTR (copy (&a)) short m_ara;
  ATTR (copy (&b.a)) int m_arb_a;
  ATTR (copy (*b.pa)) long m_xb_pa;
  ATTR (copy (b.pa[0])) long m_arb_pa;

  ATTR (copy (*pa)) short m_xpa;
  ATTR (copy (pa[0])) short m_arpa;

  ATTR (copy (ra)) short m_ra;

  ATTR (copy (ab[0].a)) int m_arab_a;
  ATTR (copy (ab[1].pa)) long m_arab_pa;
  ATTR (copy (*ab[2].pa)) int m_xarab_pa;
  ATTR (copy (ab[3].pa->bf)) unsigned int m_arab_pa_bf: 1;
  ATTR (copy (ab[4].ra.bf)) unsigned int m_arab_ra_bf: 1;

  ATTR (copy (pb->a)) int m_pb_a;
  ATTR (copy (pb->pa)) long m_pb_pa;
  ATTR (copy (*pb->pa)) int m_xpb_pa;
  ATTR (copy (pb->pa->bf)) unsigned int m_pb_pa_bf: 1;
  ATTR (copy (pb->ra.bf)) unsigned int m_pb_ra_bf: 1;

  ATTR (copy (rb.a)) int m_rb_a;
  ATTR (copy (rb.pa)) long m_rb_pa;
  ATTR (copy (*rb.pa)) int m_xrb_pa;
  ATTR (copy (rb.pa->bf)) unsigned int m_rb_pa_bf: 1;

  ATTR (aligned (4), copy ((struct A *)(0))) short m_a4_pa_0;
} C;


static_assert (__builtin_has_attribute (((C*)0)->m_pa_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_pa_1_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_pa_0_1, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_xpa_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_xpa_1_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_xpa_0_1, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_ra, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_arpa_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arpa_1_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arpa_0_1, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_a, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_b_a, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_b_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_b_ra, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_ara, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arb_a, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_xb_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arb_pa, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_xpa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arpa, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_arab_a, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arab_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_xarab_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arab_pa_bf, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_arab_ra_bf, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_pb_a, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_pb_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_pb_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_pb_pa_bf, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_pb_ra_bf, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_rb_a, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_rb_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_xrb_pa, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_rb_pa_bf, packed));

static_assert (__builtin_has_attribute (((C*)0)->m_a4_pa_0, packed));
static_assert (__builtin_has_attribute (((C*)0)->m_a4_pa_0, aligned));
static_assert (__alignof__ (((C*)0)->m_a4_pa_0) == 4);
