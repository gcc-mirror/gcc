// Compiler should not generate too many lexical blocks for this function.
// { dg-do compile { target { i?86-*-* x86_64-*-* } } }
// { dg-options "-O0 -fno-exceptions -g -dA" }

union UElement {
    void* pointer;
    int integer;
};
struct UColToken {
  unsigned source;
  unsigned char **rulesToParseHdl;
};

int uhash_hashTokens(const union UElement k)
{
  int hash = 0;
  struct UColToken *key = (struct UColToken *)k.pointer;
  if (key != 0) {
    int len = (key->source & 0xFF000000)>>24;
    int inc = ((len - 32) / 32) + 1;
    const unsigned char *p = (key->source & 0x00FFFFFF)
			     + *(key->rulesToParseHdl);
    const unsigned char *limit = p + len;
    hash = *p + *limit;
  }
  return hash;
}

// { dg-final { scan-assembler-not "LBB10" } }
