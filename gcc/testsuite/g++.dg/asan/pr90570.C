/* PR sanitizer/90570 */
/* { dg-do run } */

#include <vector>

struct stru
{
  std::vector<int> v{1,2,3,4};
  int i{5};
};

int main()
{
  stru s1;
  stru s2;

  return 0;
}
