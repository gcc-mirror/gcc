// { dg-require-effective-target c++11 }
// { dg-require-effective-target int32plus }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>
#include <tuple>
#include <algorithm>

int main()
{
  const int n = 4;
  std::vector<std::tuple<int,int,double>> vec
      = { { 1597201307, 1817606674, 0. },
            { 1380347796, 1721941769, 0.},
            {837975613, 1032707773, 0.},
            {1173654292, 2020064272, 0.} } ;
  int sup1 = 0;
  for(int i=0;i<n;++i)
    sup1=std::max(sup1,std::max(std::get<0>(vec[i]),std::get<1>(vec[i])));
  int sup2 = 0;
  for(int i=0;i<n;++i)
    sup2=std::max(std::max(sup2,std::get<0>(vec[i])),std::get<1>(vec[i]));
  if (sup1 != sup2)
    std::abort ();
  return 0;
}
