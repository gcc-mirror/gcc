/* PR target/59843 ICE on function taking/returning vector of one float64_t.  */

/* { dg-do compile } */
/* { dg-options "-Warray-bounds -O2 -fno-inline -std=c99" } */

#define TEST(BASETYPE, VECTYPE, SUFFIX)					     \
  typedef BASETYPE VECTYPE						     \
      __attribute__ ((__vector_size__ (sizeof (BASETYPE))));		     \
  VECTYPE								     \
  test_vadd_##SUFFIX (VECTYPE a, VECTYPE b)				     \
  {									     \
    return a + b;							     \
  }									     \
									     \
  void									     \
  test_##SUFFIX (BASETYPE val)						     \
  {									     \
    VECTYPE var = { val };						     \
    BASETYPE v0 = var[0];						     \
    BASETYPE v1 = var[1]; /* { dg-warning "index value is out of bound" } */ \
  }

TEST (double, float64x1_t, f64)

/* Original bug was for above type;
   in a nod to completeness, test other types too.  */

TEST (long long, int64x1_t, s64)

TEST (float, float32x1_t, f32)

TEST (long, longx1_t, l)

TEST (int, intx1_t, i)

TEST (short, int16x1_t, s16)

TEST (char, int8x1_t, s8)
/* PR target/59843 ICE on function taking/returning vector of one float64_t.  */

/* { dg-do compile } */
/* { dg-options "-Warray-bounds -O2 -fno-inline -std=c99" } */

#define TEST(BASETYPE, VECTYPE, SUFFIX)					     \
  typedef BASETYPE VECTYPE						     \
      __attribute__ ((__vector_size__ (sizeof (BASETYPE))));		     \
  VECTYPE								     \
  test_vadd_##SUFFIX (VECTYPE a, VECTYPE b)				     \
  {									     \
    return a + b;							     \
  }									     \
									     \
  void									     \
  test_##SUFFIX (BASETYPE val)						     \
  {									     \
    VECTYPE var = { val };						     \
    BASETYPE v0 = var[0];						     \
    BASETYPE v1 = var[1]; /* { dg-warning "index value is out of bound" } */ \
  }

TEST (double, float64x1_t, f64)

/* Original bug was for above type;
   in a nod to completeness, test other types too.  */

TEST (long long, int64x1_t, s64)

TEST (float, float32x1_t, f32)

TEST (long, longx1_t, l)

TEST (int, intx1_t, i)

TEST (short, int16x1_t, s16)

TEST (char, int8x1_t, s8)
