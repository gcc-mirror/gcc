/* { dg-do compile } */

typedef unsigned char uint8_t;
typedef unsigned long int uint64_t;
union unaligned_64 {
    uint64_t l;
}
__attribute__((packed)) __attribute__((may_alias));
typedef struct AVDES {
    uint64_t round_keys[3][16];
} AVDES;
static const uint8_t PC1_shuffle[] = {
    64-57,64-49,64-41,64-33,64-25,64-17,64-9,     64-1,64-58,64-50,64-42,64-34,64-26,64-18,     64-10,64-2,64-59,64-51,64-43,64-35,64-27,     64-19,64-11,64-3,64-60,64-52,64-44,64-36,     64-63,64-55,64-47,64-39,64-31,64-23,64-15,     64-7,64-62,64-54,64-46,64-38,64-30,64-22,     64-14,64-6,64-61,64-53,64-45,64-37,64-29,     64-21,64-13,64-5,64-28,64-20,64-12,64-4 };
static const uint8_t PC2_shuffle[] = {
    56-14,56-17,56-11,56-24,56-1,56-5,     56-3,56-28,56-15,56-6,56-21,56-10,     56-23,56-19,56-12,56-4,56-26,56-8,     56-16,56-7,56-27,56-20,56-13,56-2,     56-41,56-52,56-31,56-37,56-47,56-55,     56-30,56-40,56-51,56-45,56-33,56-48,     56-44,56-49,56-39,56-56,56-34,56-53,     56-46,56-42,56-50,56-36,56-29,56-32 };
static uint64_t shuffle(uint64_t in, const uint8_t *shuffle, int shuffle_len)
{
  int i;
  uint64_t res = 0;
  for (i = 0; i < shuffle_len; i++)
    res += res + ((in >> *shuffle++) & 1);
  return res;
}
void gen_roundkeys(uint64_t K[16], uint64_t key)
{
  int i;
  uint64_t CDn = shuffle(key, PC1_shuffle, sizeof(PC1_shuffle));
  for (i = 0; i < 16; i++)
    K[i] = shuffle(CDn, PC2_shuffle, sizeof(PC2_shuffle));
}
