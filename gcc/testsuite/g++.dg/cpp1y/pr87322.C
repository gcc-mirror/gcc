// { dg-do compile { target c++14 } }

#include <array>
#include <algorithm>

int main()
{
  constexpr std::array<std::array<double,2>,3> my_mat { 
     { { 1., 1. },
       { 1., 1. },
       { 1., 1. }, }
  };
  
  std::for_each(my_mat.begin(), my_mat.end(), [
      inner_func =  [] (auto a, auto b) { return a + b; } ](auto& row) {
    std::for_each(row.begin(), row.end(), [&,
      inner_func2 =  [] (auto a, auto b) { return a + b; } ]
      (const double&) {
        return;
    });
  }); 
  
}
