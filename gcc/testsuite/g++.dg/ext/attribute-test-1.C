// { dg-do run }
// { dg-options "" }
// PR c++/13989

extern "C" void abort();

typedef unsigned uint32_t __attribute__((mode (__SI__)));

#define vector __attribute__((vector_size(16)))

struct Constants {
   inline vector uint32_t deadbeef(void) const {
       return (vector uint32_t){0xdeadbeef, 0xabababab, 0x55555555, 0x12345678};
   };
};

inline vector uint32_t const_deadbeef(Constants &C)
{
  return C.deadbeef();
}

union u {
              uint32_t f[4];
              vector uint32_t v;
} data;

int main()
{
  Constants c;
  data.v = const_deadbeef(c);
  
  if (data.f[0] != 0xdeadbeef || data.f[1] != 0xabababab 
      || data.f[2] != 0x55555555 || data.f[3] != 0x12345678)
    abort();

  return 0;
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector returned by reference.*" } */
/* { dg-prune-output "changes the ABI" } */
