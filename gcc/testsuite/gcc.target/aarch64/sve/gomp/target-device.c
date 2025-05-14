/* { dg-do compile } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS

int64_t __attribute__ ((noipa))
target_device_ptr_vla (svbool_t vp, svint32_t *vptr)
{

  int a[N], b[N], c[N];
  svint32_t va, vb, vc;
  int64_t res;
  int i;

#pragma omp parallel for
  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }
/* { dg-error {SVE type 'svint32_t \*' not allowed in 'target' device clauses} "" { target *-*-* } .+1 } */
#pragma omp target data use_device_ptr (vptr) map (to: b, c)
/* { dg-error {SVE type 'svint32_t \*' not allowed in 'target' device clauses} "" { target *-*-* } .+1 } */
#pragma omp target is_device_ptr (vptr) map (to: b, c) map (from: res)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      vb = *vptr;
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+2 } */
      /* { dg-error "cannot reference 'svbool_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      vc = svld1_s32 (vp, c);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      va = svadd_s32_z (vp, vb, vc);
      res = svaddv_s32 (svptrue_b32 (), va);
    }

  return res;
}

int64_t __attribute__ ((noipa))
target_device_addr_vla (svbool_t vp, svint32_t *vptr)
{

  int a[N], b[N], c[N];
  svint32_t va, vb, vc;
  int64_t res;
  int i;

#pragma omp parallel for
  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'target' device clauses" "" {  target *-*-* } .+1 } */
#pragma omp target data use_device_addr (vb) map (to: b, c)
/* { dg-error {SVE type 'svint32_t \*' not allowed in 'target' device clauses} "" { target *-*-* } .+1 } */
#pragma omp target is_device_ptr (vptr) map (to: b, c) map (from: res)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      vb = *vptr;
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+2 } */
      /* { dg-error "cannot reference 'svbool_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      vc = svld1_s32 (vp, c);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      va = svadd_s32_z (vp, vb, vc);
      res = svaddv_s32 (svptrue_b32 (), va);
    }

  return res;
}

int64_t __attribute__ ((noipa))
target_has_device_addr_vla (svbool_t vp, svint32_t *vptr)
{

  int a[N], b[N], c[N];
  svint32_t va, vb, vc;
  int64_t res;
  int i;

#pragma omp parallel for
  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

/* { dg-error "SVE type 'svint32_t' not allowed in 'target' device clauses" "" {  target *-*-* } .+1 } */
#pragma omp target data use_device_addr (vb) map (to: b, c)
/* { dg-error "SVE type 'svint32_t' not allowed in 'target' device clauses" "" {  target *-*-* } .+1 } */
#pragma omp target has_device_addr (vb) map (to: b, c) map (from: res)
  for (i = 0; i < 8; i++)
    {
      /* { dg-error "cannot reference 'svbool_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      vb = svld1_s32 (vp, b);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      vc = svld1_s32 (vp, c);
      /* { dg-error "cannot reference 'svint32_t' object types in 'target' region" "" { target *-*-* } .+1 } */
      va = svadd_s32_z (vp, vb, vc);
      res = svaddv_s32 (svptrue_b32 (), va);
    }

  return res;
}

#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))

typedef __SVInt32_t v8si FIXED_ATTR;
typedef svbool_t v8bi FIXED_ATTR;

int64_t __attribute__ ((noipa))
target_device_ptr_vls (v8bi vp, v8si *vptr)
{

  int a[N], b[N], c[N];
  v8si va, vb, vc;
  int64_t res;
  int i;

#pragma omp parallel for
  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp target data use_device_ptr (vptr) map (to: b, c)
#pragma omp target is_device_ptr (vptr) map (to: b, c) map (from: res)
  for (i = 0; i < 8; i++)
    {
      vb = *vptr;
      vc = svld1_s32 (vp, c);
      va = svadd_s32_z (vp, vb, vc);
      res = svaddv_s32 (svptrue_b32 (), va);
    }

  return res;
}

int64_t __attribute__ ((noipa))
target_device_addr_vls (v8bi vp, v8si *vptr)
{

  int a[N], b[N], c[N];
  v8si va, vb, vc;
  int64_t res;
  int i;

#pragma omp parallel for
  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp target data use_device_addr (vb) map (to: b, c)
#pragma omp target is_device_ptr (vptr) map (to: b, c) map (from: res)
  for (i = 0; i < 8; i++)
    {
      vb = *vptr;
      vc = svld1_s32 (vp, c);
      va = svadd_s32_z (vp, vb, vc);
      res = svaddv_s32 (svptrue_b32 (), va);
    }

  return res;
}

int64_t __attribute__ ((noipa))
target_has_device_addr_vls (v8bi vp, v8si *vptr)
{

  int a[N], b[N], c[N];
  v8si va, vb, vc;
  int64_t res;
  int i;

#pragma omp parallel for
  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp target data use_device_addr (vb) map (to: b, c)
#pragma omp target has_device_addr (vb) map (to: b, c) map (from: res)
  for (i = 0; i < 8; i++)
    {
      vb = svld1_s32 (vp, b);
      vc = svld1_s32 (vp, c);
      va = svadd_s32_z (vp, vb, vc);
      res = svaddv_s32 (svptrue_b32 (), va);
    }

  return res;
}
