// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import Two;

int main ()
{
  X x (0xdead, 0xbeef);

  if (x.a != 0xdead || x.b != 0xbeef)
    return 1;

  Frob (x);
  if (x.b != 0xdead)
    return 2;

  X y (0xcafe);
  if (y.a != 0xcafe || y.b != 0xcafe << 16)
    return 3;

  return 0;
}

// { dg-final { scan-lang-dump {Imported:-1 type_decl:'::X@One:.'@One} module } }
// { dg-final { scan-lang-dump {Indirect:-2 decl's type record_type:'::X@One:.'} module } }
// { dg-final { scan-lang-dump {Read member:-[0-9]* field_decl:'::X@One:.::a'} module } }
// { dg-final { scan-lang-dump {Read member:-[0-9]* field_decl:'::X@One:.::b'} module } }
