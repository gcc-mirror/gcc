/* This testcase checks that the shift operand of r*sbg instructions is in
   range.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -Wno-shift-count-overflow" } */

int g;

void pos_ll_129 (long long b)
{
  if (b >> 129 & 1)
    g = b;
}

void sizepos_ll_134 (long long b)
{
  if (b >> 134 & 1)
    g = b;
}

void pos_ll_65 (long long b)
{
  if (b >> 65 & 1)
    g = b;
}

void sizepos_ll_70 (long long b)
{
  if (b >> 70 & 1)
    g = b;
}

void pos_ll_33 (long long b)
{
  if (b >> 33 & 1)
    g = b;
}

void sizepos_ll_38 (long long b)
{
  if (b >> 38 & 1)
    g = b;
}

void pos_ll_17 (long long b)
{
  if (b >> 17 & 1)
    g = b;
}

void sizepos_ll_22 (long long b)
{
  if (b >> 22 & 1)
    g = b;
}

void pos_ll_8 (long long b)
{
  if (b >> 8 & 1)
    g = b;
}

void sizepos_ll_13 (long long b)
{
  if (b >> 13 & 1)
    g = b;
}

void pos_l_129 (long b)
{
  if (b >> 129 & 1)
    g = b;
}

void sizepos_l_134 (long b)
{
  if (b >> 134 & 1)
    g = b;
}

void pos_l_65 (long b)
{
  if (b >> 65 & 1)
    g = b;
}

void sizepos_l_70 (long b)
{
  if (b >> 70 & 1)
    g = b;
}

void pos_l_33 (long b)
{
  if (b >> 33 & 1)
    g = b;
}

void sizepos_l_38 (long b)
{
  if (b >> 38 & 1)
    g = b;
}

void pos_l_17 (long b)
{
  if (b >> 17 & 1)
    g = b;
}

void sizepos_l_22 (long b)
{
  if (b >> 22 & 1)
    g = b;
}

void pos_l_8 (long b)
{
  if (b >> 8 & 1)
    g = b;
}

void sizepos_l_13 (long b)
{
  if (b >> 13 & 1)
    g = b;
}

void pos_i_129 (int b)
{
  if (b >> 129 & 1)
    g = b;
}

void sizepos_i_134 (int b)
{
  if (b >> 134 & 1)
    g = b;
}

void pos_i_65 (int b)
{
  if (b >> 65 & 1)
    g = b;
}

void sizepos_i_70 (int b)
{
  if (b >> 70 & 1)
    g = b;
}

void pos_i_33 (int b)
{
  if (b >> 33 & 1)
    g = b;
}

void sizepos_i_38 (int b)
{
  if (b >> 38 & 1)
    g = b;
}

void pos_i_17 (int b)
{
  if (b >> 17 & 1)
    g = b;
}

void sizepos_i_22 (int b)
{
  if (b >> 22 & 1)
    g = b;
}

void pos_i_8 (int b)
{
  if (b >> 8 & 1)
    g = b;
}

void sizepos_i_13 (int b)
{
  if (b >> 13 & 1)
    g = b;
}

void pos_s_129 (short b)
{
  if (b >> 129 & 1)
    g = b;
}

void sizepos_s_134 (short b)
{
  if (b >> 134 & 1)
    g = b;
}

void pos_s_65 (short b)
{
  if (b >> 65 & 1)
    g = b;
}

void sizepos_s_70 (short b)
{
  if (b >> 70 & 1)
    g = b;
}

void pos_s_33 (short b)
{
  if (b >> 33 & 1)
    g = b;
}

void sizepos_s_38 (short b)
{
  if (b >> 38 & 1)
    g = b;
}

void pos_s_17 (short b)
{
  if (b >> 17 & 1)
    g = b;
}

void sizepos_s_22 (short b)
{
  if (b >> 22 & 1)
    g = b;
}

void pos_s_8 (short b)
{
  if (b >> 8 & 1)
    g = b;
}

void sizepos_s_13 (short b)
{
  if (b >> 13 & 1)
    g = b;
}

void pos_c_129 (signed char b)
{
  if (b >> 129 & 1)
    g = b;
}

void sizepos_c_134 (signed char b)
{
  if (b >> 134 & 1)
    g = b;
}

void pos_c_65 (signed char b)
{
  if (b >> 65 & 1)
    g = b;
}

void sizepos_c_70 (signed char b)
{
  if (b >> 70 & 1)
    g = b;
}

void pos_c_33 (signed char b)
{
  if (b >> 33 & 1)
    g = b;
}

void sizepos_c_38 (signed char b)
{
  if (b >> 38 & 1)
    g = b;
}

void pos_c_17 (signed char b)
{
  if (b >> 17 & 1)
    g = b;
}

void sizepos_c_22 (signed char b)
{
  if (b >> 22 & 1)
    g = b;
}

void pos_c_8 (signed char b)
{
  if (b >> 8 & 1)
    g = b;
}

void sizepos_c_13 (signed char b)
{
  if (b >> 13 & 1)
    g = b;
}
