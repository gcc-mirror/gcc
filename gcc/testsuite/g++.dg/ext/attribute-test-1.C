// { dg-do run }
// { dg-options "" }
// PR c++/13989

extern "C" void abort();

#define vector __attribute__((vector_size(16)))

struct Constants {
   inline vector unsigned int deadbeef(void) const { // { dg-warning "vector returned by ref" "" { target { powerpc*-*-linux* && ilp32 } } }
       return (vector unsigned int){0xdeadbeef, 0xabababab, 0x55555555, 0x12345678};
   };
};

inline vector unsigned int const_deadbeef(Constants &C)
{
  return C.deadbeef();
}

union u {
              unsigned int f[4];
              vector unsigned int v;
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


