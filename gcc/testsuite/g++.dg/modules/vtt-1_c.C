// { dg-additional-options "-fmodules-ts -O2 -fno-inline" }

import bar;

int main ()
{
  make_foo ();
  make_bar ();

  mine m0;

  return 0;
}

// { dg-final {scan-assembler-not "_ZTV4base:" } }
// { dg-final {scan-assembler-not "_ZTV7derived:" } }
// { dg-final {scan-assembler-not "_ZTT7derived:" } }
// { dg-final {scan-assembler-not "_ZTV4mine:" } }
// { dg-final {scan-assembler-not "_ZTT4mine:" } }
