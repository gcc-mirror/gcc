/* { dg-do compile } */
/* { dg-options "-O2 -mavx -std=c++11" } */

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
