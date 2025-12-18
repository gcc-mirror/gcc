// { dg-do compile { target float16 } }

// Test name mangling

void f1 (_Float16) {}
void f2 (_Float16 *) {}
void f3 (_Float16 const *) {}

// { dg-final { scan-assembler "_Z2f1DF16_:" } }
// { dg-final { scan-assembler "_Z2f2PDF16_:" } }
// { dg-final { scan-assembler "_Z2f3PKDF16_:" } }
