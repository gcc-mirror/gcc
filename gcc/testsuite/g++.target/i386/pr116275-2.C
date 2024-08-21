/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mavx -mno-avx512vl -mstv -mno-stackrealign -std=c++11" } */

struct SymbolDesc push_back(SymbolDesc);
struct SymbolDesc {
  long long ELFLocalSymIdx;
};
struct Expected {
  long long &operator*();
};
void SymbolizableObjectFileaddSymbol() {
  Expected SymbolAddressOrErr;
  long long SymbolAddress = *SymbolAddressOrErr << 8 >> 8;
  push_back({SymbolAddress});
}

/* { dg-final { scan-assembler "vpslld" } } */
/* { dg-final { scan-assembler-not "vpsllq" } } */
/* { dg-final { scan-assembler-not "vpsrlq" } } */
