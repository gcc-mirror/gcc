// { dg-do run }
// { dg-options "-Wno-abi" }
// PR c++/9844

extern "C" void abort();

#define vector __attribute__((vector_size(16)))

class vector_holder
{
   char __attribute__((vector_size(16))) vec;
   char __attribute__((vector_size(16))) vec1;
public:
   operator __attribute__((vector_size(16))) short (void) {
     return (__attribute__((vector_size(16))) short) vec;
   }

   operator __attribute__((vector_size(16))) unsigned int (void) {
     return (__attribute__((vector_size(16))) unsigned int) vec1;
   }

   vector_holder () {
	vec = (__attribute__((vector_size(16))) char) {'a', 'b', 'c', 'd', 'a', 'b', 'c', 'd',
						       'a', 'b', 'c', 'd', 'a', 'b', 'c', 'd'};
	vec1 = (__attribute__((vector_size(16))) char) {'m', 'n', 'o', 'q', 'm', 'n', 'o', 'p',
							'm', 'n', 'o', 'q', 'm', 'n', 'o', 'p'};
   }
};

union u {
              char f[16];
              vector unsigned int v;
              vector short vs;
} data;


vector_holder vh;

int main()
{
  data.vs = (__attribute__((vector_size(16))) short) vh;
  if (data.f[0] != 'a' || data.f[15] != 'd')
    abort(); 
  data.v = (__attribute__((vector_size(16))) unsigned int) vh;
  if (data.f[0] != 'm' || data.f[15] != 'p')
    abort(); 

  return 0;
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector returned by reference.*" } */
/* { dg-prune-output "changes the ABI" } */
