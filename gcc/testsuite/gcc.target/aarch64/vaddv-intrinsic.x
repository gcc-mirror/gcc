
float32_t
test_vaddv_v2sf (const float32_t *pool)
{
  float32x2_t val;

  val = vld1_f32 (pool);
  return vaddv_f32 (val);
}

float32_t
test_vaddv_v4sf (const float32_t *pool)
{
  float32x4_t val;

  val = vld1q_f32 (pool);
  return vaddvq_f32 (val);
}

float64_t
test_vaddv_v2df (const float64_t *pool)
{
  float64x2_t val;

  val = vld1q_f64 (pool);
  return vaddvq_f64 (val);
}
