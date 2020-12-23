// { dg-additional-options "-fmodules-ts -fno-inline" }

import Foo;

int main ()
{
  GMF ();
  Bill::dob ();
  Bill::frob ();

  return 0;
}

// { dg-final { scan-assembler {_ZN3Bob4frobEv:} } }
// { dg-final { scan-assembler {_ZN4Bill4frobEv:} } }
// { dg-final { scan-assembler-not {_ZN4Bill3dobEv:} } }
// { dg-final { scan-assembler {_Z3GMFv:} } }

// { dg-final { scan-assembler {call[ \t]+_?_ZN3Bob4frobEv} { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler {call[ \t]+_?_Z3GMFv} { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler {call[ \t]+_?_ZN4Bill3dobEv} { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler {call[ \t]+_?_ZN4Bill4frobEv} { target i?86-*-* x86_64-*-* } } }
