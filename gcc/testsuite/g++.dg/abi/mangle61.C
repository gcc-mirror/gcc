// PR c++/56237
// { dg-do compile }
// { dg-additional-options -fabi-compat-version=0 }

void *p[4];

void
foo ()
{
  static union { } u;
  p[0] = &u;
  {
    static union { } u; 
    p[1] = &u;
    {
      static union { } u;
      p[2] = &u;
    }
  }
  {
    static union { } u;
    p[3] = &u;
  }
}

// { dg-final { scan-assembler "_ZZ3foovE1u\[^_\]" } }
// { dg-final { scan-assembler "_ZZ3foovE1u_0" } }
// { dg-final { scan-assembler "_ZZ3foovE1u_1" } }
// { dg-final { scan-assembler "_ZZ3foovE1u_2" } }
