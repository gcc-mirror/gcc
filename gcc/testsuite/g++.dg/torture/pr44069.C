/* { dg-do run } */

template <unsigned R, unsigned C>
class M {
public:
    M(const int* arr) {
	for (unsigned long r = 0; r < R; ++r)
	  for (unsigned long c = 0; c < C; ++c)
	    m[r*C+c] = arr[r*C+c];
    }
    int operator()(unsigned r, unsigned c) const
      { return m[r*C+c]; }
private:
    int m[R*C];
};
extern "C" void abort (void);
int main()
{
  int vals[2][2] = { { 1, 2 }, { 5, 6 } };
  M<2,2> m( &(vals[0][0]) );
  if (m(1,0) != 5)
    abort ();
  return 0;
}

