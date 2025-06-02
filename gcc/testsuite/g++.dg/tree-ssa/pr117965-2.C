// { dg-do compile { target c++17 } }
// { dg-options "-O2 -fdump-tree-phiopt1" }

#include <iostream>
#include <algorithm>

void clamp2 ()
{
  float low = -1.0f, high = 1.0f;
  float num1, num2, num3;
  std::cin >> num1;
  num1 = std::clamp(num1, low, high);
  std::cout << num1;
}

// { dg-final { scan-tree-dump-times " < -1.0" 1 "phiopt1" } }
// { dg-final { scan-tree-dump-times " \\\? -1.0e\\\+0 : " 1 "phiopt1" } }
// { dg-final { scan-tree-dump-times " > 1.0" 1 "phiopt1" } }
// { dg-final { scan-tree-dump-times " \\\? 1.0e\\\+0 : " 1 "phiopt1" } }
