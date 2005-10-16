template<int dummy>
    struct X
    {
      static const int n_primes = 256;
      static const unsigned long primes[n_primes + 1];
    };

  template<int dummy>
  const int X<dummy>::n_primes;

  template<int dummy>
  const unsigned long X<dummy>::primes[n_primes + 1] =
    { 0 };


const unsigned long  *f1(void){return &X<0>::primes[0];}
int main(){}
