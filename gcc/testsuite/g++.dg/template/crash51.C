// PR c++/26496

template< typename _Generator> int generate_n(_Generator __gen);
struct Distribution { };
typedef double (Distribution::* Pstd_mem)();
int main(void)
{
  Distribution* rng;
  Pstd_mem ptr;
  generate_n(rng->*ptr); // { dg-error "non-static member" } 
}
