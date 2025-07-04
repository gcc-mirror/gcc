typedef __UINT8_TYPE__ uint8_t;
typedef __UINT32_TYPE__ uint32_t;

typedef struct
{
  uint32_t dword[2];
  uint8_t byte[8];
} reg64_t;
reg64_t TestF20F_opgd, TestF20F_oped;

void
TestF20F ()
{
  TestF20F_opgd.dword[0] ^= TestF20F_oped.byte[0];
  for (int i = 0; i < 8; i++)
    if (TestF20F_opgd.dword[0] & 1)
      TestF20F_opgd.dword[0] = TestF20F_opgd.dword[0] >> 1 ^ (uint32_t)2197175160UL;
    else
      TestF20F_opgd.dword[0] = TestF20F_opgd.dword[0] >> 1;
}
