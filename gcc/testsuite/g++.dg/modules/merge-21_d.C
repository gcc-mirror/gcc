// PR c++/124431
// { dg-additional-options "-fmodules -fdump-lang-module-alias" }

import C;
import B;

void test() {
  constexpr auto r = mat<4>()[0];
  // { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) field_decl:'::mat@.:.<0x4>::data'} module } }

  S<0> s;
  s.field;
  // { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) field_decl:'::S@.:.<0x0>::field'} module } }

  S<0>::e se = S<0>::a;
  // { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) const_decl:'::S@.:.<0x0>::e@.:.<0x0>::a'} module } }

  S<0>::ce sce = S<0>::ce::ca;
  // { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) const_decl:'::S@.:.<0x0>::ce@.:.<0x0>::ca'} module } }
}
