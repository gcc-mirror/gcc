// PR c++/23293

template < typename > struct P;
struct S;

void *unrelated_function()
{
  typedef S K;
  P < K > * p;
  return p;
}

template < typename U >
void generate_warning()
{ 
  U::x(); // { dg-error "P<S>" }
}

int main()
{
  generate_warning< P < S > >();
}
