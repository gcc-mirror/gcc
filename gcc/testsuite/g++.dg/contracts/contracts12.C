// ensure that constants for contract levels are inserted into the binary when
// used and omitted when the runtime check is not generated
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontract-build-level=default" }
// { dg-final { scan-assembler-not "audit" } }
// { dg-final { scan-assembler "default" } }

int main()
{
  int x = 1;
  [[assert: x < 0]];
  [[assert default: x < 0]];
  [[assert audit: x < 0]];
  return 0;
}
