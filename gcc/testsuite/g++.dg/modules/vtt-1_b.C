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

// { dg-final {scan-assembler-not "_ZTVW3foo4base:" } }
// { dg-final {scan-assembler-not "_ZTVW3foo7derived:" } }
// { dg-final {scan-assembler-not "_ZTTW3foo7derived:" } }
// { dg-final {scan-assembler "_ZTVW3bar4mine:" } }
// { dg-final {scan-assembler "_ZTTW3bar4mine:" } }
