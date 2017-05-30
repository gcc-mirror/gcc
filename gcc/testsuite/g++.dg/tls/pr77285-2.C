// PR c++/77285
// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }
// { dg-final { scan-assembler "_Z4var1B3tag" } }
// { dg-final { scan-assembler "_Z4var2B3tag" } }
// { dg-final { scan-assembler "_ZTH4var1B3tag" } }
// { dg-final { scan-assembler "_ZTW4var1B3tag" } }

struct __attribute__((abi_tag("tag"))) X { ~X () {} int i = 0; };
extern thread_local X var1;
extern X var2;

int
main ()
{
 return var1.i + var2.i;
}
