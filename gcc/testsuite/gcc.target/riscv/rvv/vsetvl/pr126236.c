/* PR middle-end/126236 */
/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O2 -fduplicate-switch-dispatch -mno-explicit-relocs -fdump-rtl-compgotos" } */

/* Execute each ordered pair of handlers with different vector configurations.
   The reference uses the same unsigned operations without vectorization.  */

struct state
{
  unsigned char byte[128];
  unsigned short half[64];
  unsigned int word[32];
  unsigned long long wide[16];
};

static struct state actual __attribute__((aligned (64)));
static struct state expected __attribute__((aligned (64)));

#define DISPATCH_BODY                                                    \
  for (;;)                                                              \
    switch (*pc++)                                                       \
      {                                                                 \
      case 0: return;                                                   \
      case 1:                                                           \
        for (unsigned i = 0; i < 128; ++i)                               \
          s->byte[i] += 3;                                               \
        break;                                                          \
      case 2:                                                           \
        for (unsigned i = 0; i < 64; ++i)                                \
          s->half[i] ^= 0x1357;                                          \
        break;                                                          \
      case 3:                                                           \
        for (unsigned i = 0; i < 32; ++i)                                \
          s->word[i] += 0x1234567u;                                      \
        break;                                                          \
      case 4:                                                           \
        for (unsigned i = 0; i < 16; ++i)                                \
          s->wide[i] ^= 0x13579bdf02468aceull;                            \
        break;                                                          \
      case 5:                                                           \
        for (unsigned i = 0; i < 57; ++i)                                \
          s->byte[i] ^= 0x5a;                                            \
        break;                                                          \
      case 6:                                                           \
        for (unsigned i = 0; i < 31; ++i)                                \
          s->half[i] += 71;                                              \
        break;                                                          \
      case 7:                                                           \
        for (unsigned i = 0; i < 13; ++i)                                \
          s->word[i] ^= 0x2468ace0u;                                     \
        break;                                                          \
      case 8:                                                           \
        for (unsigned i = 0; i < 5; ++i)                                 \
          s->wide[i] += 0x102030405060708ull;                            \
        break;                                                          \
      default: return;                                                  \
      }

__attribute__((noinline, noclone))
static void
vector_dispatch (struct state *s, const unsigned char *pc)
{
  DISPATCH_BODY
}

__attribute__((noinline, noclone, optimize ("no-tree-vectorize")))
static void
scalar_dispatch (struct state *s, const unsigned char *pc)
{
  DISPATCH_BODY
}

int
main (void)
{
  unsigned char code[129];
  unsigned pos = 0;
  for (unsigned first = 1; first <= 8; ++first)
    for (unsigned second = 1; second <= 8; ++second)
      {
        code[pos++] = first;
        code[pos++] = second;
      }
  code[pos] = 0;

  for (unsigned i = 0; i < 128; ++i)
    actual.byte[i] = expected.byte[i] = i * 7 + 1;
  for (unsigned i = 0; i < 64; ++i)
    actual.half[i] = expected.half[i] = i * 257 + 3;
  for (unsigned i = 0; i < 32; ++i)
    actual.word[i] = expected.word[i] = i * 0x1234567u + 5;
  for (unsigned i = 0; i < 16; ++i)
    actual.wide[i] = expected.wide[i] = i * 0x123456789abcdefull + 7;

  for (unsigned round = 0; round < 12; ++round)
    {
      vector_dispatch (&actual, code);
      scalar_dispatch (&expected, code);
    }

  for (unsigned i = 0; i < 128; ++i)
    if (actual.byte[i] != expected.byte[i])
      return 1;
  for (unsigned i = 0; i < 64; ++i)
    if (actual.half[i] != expected.half[i])
      return 2;
  for (unsigned i = 0; i < 32; ++i)
    if (actual.word[i] != expected.word[i])
      return 3;
  for (unsigned i = 0; i < 16; ++i)
    if (actual.wide[i] != expected.wide[i])
      return 4;
  return 0;
}

/* { dg-final { scan-rtl-dump "Duplicating dispatch" "compgotos" { target { no-opts "-flto" } } } } */
/* { dg-final { scan-rtl-dump {addr_(?:diff_)?vec/u} "compgotos" { target { no-opts "-flto" } } } } */
