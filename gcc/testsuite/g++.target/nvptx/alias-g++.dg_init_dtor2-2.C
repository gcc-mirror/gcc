/* Reduced from 'g++.dg/init/dtor2.C'.  */

/* { dg-do link } */
/* { dg-additional-options -mno-alias } */
/* { dg-additional-options -save-temps } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

struct B
{
  ~B();
};

B::~B () {
}

int main()
{
  B b;
  return 0;
}

/* { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DECL: _ZN1BD2Ev$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func _ZN1BD2Ev \(\.param\.u64 %in_ar0\);$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DEF: _ZN1BD2Ev$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func _ZN1BD2Ev \(\.param\.u64 %in_ar0\)$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DECL: _ZN1BD1Ev$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func _ZN1BD1Ev \(\.param\.u64 %in_ar0\);$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DEF: _ZN1BD1Ev$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func _ZN1BD1Ev \(\.param\.u64 %in_ar0\)$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)\tcall _ZN1BD1Ev, \(} 1 } }
   { dg-final { scan-assembler-times {(?n)\tcall _ZN1BD2Ev, \(} 0 } } */
