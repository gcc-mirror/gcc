// Error: Internal compiler error on egcs 1998/05/28 snapshot.

const double M_PI=3.14159265358979323846;

template<int N,int I,int J,int K>
inline double SineSeries()
{
  const double x=I*2*M_PI/N;
  const bool go=K+1!=J;
  return 1.0-x*x/(2*K+2)/(2*K+3)*SineSeries<N*go,I*go,J*go,(K+1)*go>();
}

template<>
inline double SineSeries<0,0,0,0>()
{
  return 1.0;
}

template<int N,int I>
inline double Sine()
{
  const double x=(I*2*M_PI/N);
  return x * SineSeries<N,I,10,0>();
}

int main()
{
  double f=Sine<32,5>()
  return 0;               // ERROR - parse error
}
