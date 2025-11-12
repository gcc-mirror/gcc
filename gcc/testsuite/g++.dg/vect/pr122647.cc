// { dg-do compile }

void av(float *au)
{
  for (int i = 0; i < 1024; ++i)
  {
    float t = i;
    int tt = __builtin_bit_cast(int, t);
    bool t1 = tt;
    float t2 = t1;
    int t3 = __builtin_bit_cast(int, t2);
    bool t4 = t3;
    float t5 = t5;
    au[i] = t4;
  }
}

// { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" { target { { vect_uintfloat_cvt && vect_float } && vect_bool_cmp } } } }
