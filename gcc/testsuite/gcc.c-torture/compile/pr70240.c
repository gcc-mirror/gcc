typedef short v16hi __attribute__ ((vector_size (32)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef long long v4di __attribute__ ((vector_size (32)));

int
foo(int u16_0, int u32_0, int u64_0, int u16_1, int u32_1, int u64_1, v16hi v32u16_0, v8si v32u32_0, v4di v32u64_0, v16hi v32u16_1, v8si v32u32_1, v4di v32u64_1)
{
  do {
    v32u16_1 += (v16hi){ v32u32_1[7], ~v32u32_1[3], 0, v32u64_0[0]};
    u32_0 = (u32_0 << 31) | (u32_0 >> ~v32u32_0[1]);
    u64_0 += 1;
    v32u64_0[2] <<= v32u64_0[2] & 63;
    u16_1 = (u16_1 >> (v32u16_0[11] & 15)) | (u16_1 << (-v32u16_0[11] & 15));
    v32u16_0 -= ~v32u16_1;
    v32u32_1[5] += u32_1;
    if (v32u32_1[3] >= 0) {
      u64_1 -= ~v32u64_1[1];
      v32u16_1 += (v16hi){ -u64_1, ~u32_0, ~u16_1, v32u32_1[1], 0, ~v32u16_1[2], ~v32u64_1[2], ~v32u32_0[7]};
    }
    v32u64_1 += (v4di){0, 0, ~v32u32_0[5]};
    v32u32_1 *= (v8si){0, ~v32u32_1[6]};
    v32u64_0[3] &= 0x1234;
    v32u64_0 += (v4di){v32u32_1[6]};
  } while (u16_0 < 0x1234);
  return u64_0 + u16_1;
}
