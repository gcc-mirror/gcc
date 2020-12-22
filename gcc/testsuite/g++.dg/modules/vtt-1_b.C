// { dg-additional-options "-fmodules-ts -O2 -fno-inline" }

export module bar;
// { dg-module-cmi bar }

export import foo;

export struct mine : derived
{
  mine () {}
  ~mine ();
  int mm;
};

mine::~mine ()
{
}

export inline void make_bar ()
{
  mine m;
}

// { dg-final {scan-assembler-not "_ZTV4base:" } }
// { dg-final {scan-assembler-not "_ZTV7derived:" } }
// { dg-final {scan-assembler-not "_ZTT7derived:" } }
// { dg-final {scan-assembler "_ZTV4mine:" } }
// { dg-final {scan-assembler "_ZTT4mine:" } }
