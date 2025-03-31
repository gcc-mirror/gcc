/* { dg-do compile } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__((arm_sve_vector_bits (N)))

typedef svint32_t v8si FIXED_ATTR;
typedef svbool_t v8bi FIXED_ATTR;

void
target_vla1 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla1 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla1 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla1 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla1 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls1 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls1 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls1 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls1 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls1 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla2 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target parallel
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla2 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla2 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla2 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla2 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target parallel
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls2 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target parallel
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls2 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target parallel map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls2 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target parallel map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target parallel map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls2 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target parallel map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target parallel map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls2 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target parallel
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target parallel map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla3 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target parallel loop
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla3 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla3 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla3 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel loop map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel loop map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla3 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target parallel loop
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target parallel loop map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls3 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target parallel loop
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls3 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target parallel loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls3 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target parallel loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target parallel map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls3 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target parallel loop map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target parallel loop map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls3 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target parallel loop
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target parallel loop map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla4 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target simd
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla4 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla4 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target simd map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla4 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target simd map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target simd map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla4 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target simd
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target simd map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls4 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target simd
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls4 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls4 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target simd map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls4 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target simd map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target simd map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls4 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target simd
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target simd map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla5 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla5 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla5 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla5 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla5 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls5 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls5 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls5 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls5 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target teams map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls5 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla6 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams loop
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla6 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla6 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla6 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams loop map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams loop map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla6 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams loop
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams loop map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls6 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams loop
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls6 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls6 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams loop map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls6 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target teams loop map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams loop map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls6 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams loop
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target teams loop map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla7 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams distribute
  for (i = 0; i < 8; i++)
    {
      svint32_t va;

      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla7 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla7 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla7 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla7 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams distribute
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls7 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams distribute
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls7 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams distribute map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls7 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams distribute map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls7 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target teams distribute map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams distribute map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls7 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams distribute
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target teams distribute map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vla8 (svint32_t vb, svint32_t vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams distribute simd
  for (i = 0; i < 8; i++)
    {
      svint32_t va;
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

svint32_t
target_data_map_1_vla8 (svint32_t vb, svint32_t vc, int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

svint32_t
target_data_map_2_vla8 (svint32_t vb, svint32_t vc, int *a, int *b,
			    int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

svint32_t
target_map_data_enter_exit_vla8 (svint32_t vb, svint32_t vc, int *a,
				     int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target enter data map(to: b, c, vb, vc)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute simd map(from: va)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute simd map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target exit data map(from: va)

  return va;
}

svint32_t
target_map_data_alloc_update_vla8 (svint32_t vb, svint32_t vc, int *a,
				       int *b, int *c)
{
  svint32_t va;
  int i;

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams distribute simd
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	/* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

/* { dg-error "SVE type 'svint32_t' not allowed in 'map' clause" "" { target *-*-* } .+1 } */
#pragma omp target teams distribute simd map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

void
target_vls8 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  int i;

#pragma omp target teams distribute simd
  for (i = 0; i < 8; i++)
    {
      v8si va;
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }
}

v8si
target_data_map_1_vls8 (v8si vb, v8si vc, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams distribute simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }
  return va;
}

v8si
target_data_map_2_vls8 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target teams distribute simd map(to: b, c, vb, vc) map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}

v8si
target_map_data_enter_exit_vls8 (v8si vb, v8si vc, int *a, int *b, int *c)
{
  v8si va;
  int i;

#pragma omp target enter data map(to: b, c, vb, vc)

#pragma omp target teams distribute simd map(from: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target teams distribute simd map(to: va) map(to: a)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }

#pragma omp target exit data map(from: va)

  return va;
}

v8si
target_map_data_alloc_update_vls8 (v8si vb, v8si vc, int *a, int *b,
				       int *c)
{
  v8si va;
  int i;

#pragma omp target data map(to: b, c, vb, vc) map(alloc: va)

#pragma omp target teams distribute simd
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), b + i * 8, vb);
      svst1_s32 (svptrue_b32 (), c + i * 8, vc);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), vb, vc);
    }

#pragma omp target update from(va)

#pragma omp target teams distribute simd map(to: a) map(tofrom: va)
  for (i = 0; i < 8; i++)
    {
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
      if (i == 7)
	va = svadd_s32_z (svptrue_b32 (), va, svindex_s32 (1, 1));
    }
  return va;
}
