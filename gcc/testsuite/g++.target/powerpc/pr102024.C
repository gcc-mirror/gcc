// PR target/102024
// { dg-do compile { target powerpc_elfv2 } }
// { dg-options "-O2" }

// Test that a zero-width bit field in an otherwise homogeneous aggregate
// generates a psabi warning and passes arguments in GPRs.

// { dg-final { scan-assembler-times {\mstd\M} 4 } }

struct a_thing
{
  double x;
  double y;
  double z;
  int : 0;
  double w;
};

double
foo (a_thing a) // { dg-message "ELFv2 parameter passing for an argument containing zero-width bit fields but that is otherwise a homogeneous aggregate was corrected in GCC 12" }
{
  return a.x * a.y + a.z - a.w;
}
