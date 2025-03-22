// { dg-additional-options "-fmodules" }

import "extern-tpl-4_a.H";
import M;

int main() {
  ha<int>();
  ha<char>();
  ha<double>();

  ma<int>();
  ma<char>();
  ma<double>();

  hb<int>();
  hb<char>();
  hb<double>();

  mb<int>();
  mb<char>();
  mb<double>();

  int x1 = hc<int> + hc<char> + hc<double>;
  int x2 = hd<int> + hd<char> + hd<double>;
  int x3 = mc<int> + mc<char> + mc<double>;
  int x4 = md<int> + md<char> + md<double>;
  return x1 + x2 + x3 + x4;
}


// 'int': imported explicit instantiation decls should not be emitted here:
// { dg-final { scan-assembler-not "_Z2haIiEvv:" } }
// { dg-final { scan-assembler-not "_Z2hbIiEvv:" } }
// { dg-final { scan-assembler-not "_Z2hcIiE:" } }
// { dg-final { scan-assembler-not "_Z2hdIiE:" } }
// { dg-final { scan-assembler-not "_ZW1M2maIiEvv:" } }
// { dg-final { scan-assembler-not "_ZW1M2mbIiEvv:" } }
// { dg-final { scan-assembler-not "_ZW1M2mcIiE:" } }
// { dg-final { scan-assembler-not "_ZW1M2mdIiE:" } }

// 'char': explicit instantiation definitions don't need to be emitted for
// modules, but need to be emitted for header units (as there's no other TU):
// { dg-final { scan-assembler "_Z2haIcEvv:" } }
// { dg-final { scan-assembler "_Z2hbIcEvv:" } }
// { dg-final { scan-assembler "_Z2hcIcE:" } }
// { dg-final { scan-assembler "_Z2hdIcE:" } }
// { dg-final { scan-assembler-not "_ZW1M2maIcEvv:" } }
// { dg-final { scan-assembler-not "_ZW1M2mbIcEvv:" } }
// { dg-final { scan-assembler-not "_ZW1M2mcIcE:" } }
// { dg-final { scan-assembler-not "_ZW1M2mdIcE:" } }

// 'double': these are not explicitly instantiated and should be emitted here:
// { dg-final { scan-assembler "_Z2haIdEvv:" } }
// { dg-final { scan-assembler "_Z2hbIdEvv:" } }
// { dg-final { scan-assembler "_Z2hcIdE:" } }
// { dg-final { scan-assembler "_Z2hdIdE:" } }
// { dg-final { scan-assembler "_ZW1M2maIdEvv:" } }
// { dg-final { scan-assembler "_ZW1M2mbIdEvv:" } }
// { dg-final { scan-assembler "_ZW1M2mcIdE:" } }
// { dg-final { scan-assembler "_ZW1M2mdIdE:" } }

template void ha<bool>();
template void hb<bool>();
template int hc<bool>;
template int hd<bool>;

template void ma<bool>();
template void mb<bool>();
template int mc<bool>;
template int md<bool>;

// 'bool': instantiated in this file, and so must be emitted here:
// { dg-final { scan-assembler "_Z2haIbEvv:" } }
// { dg-final { scan-assembler "_Z2hbIbEvv:" } }
// { dg-final { scan-assembler "_Z2hcIbE:" } }
// { dg-final { scan-assembler "_Z2hdIbE:" } }
// { dg-final { scan-assembler "_ZW1M2maIbEvv:" } }
// { dg-final { scan-assembler "_ZW1M2mbIbEvv:" } }
// { dg-final { scan-assembler "_ZW1M2mcIbE:" } }
// { dg-final { scan-assembler "_ZW1M2mdIbE:" } }
