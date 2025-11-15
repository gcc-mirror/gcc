// PR c++/122625
// { dg-additional-options "-fmodules" }

import M;
int main() {
  format();
}

// { dg-final { scan-assembler {_ZNW1M4span3__vILi1EEE:} } }
// { dg-final { scan-assembler {_ZNW1M4span7partialILi5ELi0EEE:} } }
// { dg-final { scan-assembler {_ZNW1M6nestedIiE3arrIS_17integral_constantEE:} } }
// { dg-final { scan-assembler {_ZNW1M6nestedIPiE3arrIS_17integral_constantEE:} } }
// { dg-final { scan-assembler {_ZNW1M6nestedIPiE3arrIPS_17integral_constantEE:} } }
