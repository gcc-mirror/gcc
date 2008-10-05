// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR debug/37410
// { dg-do compile }

namespace A1
  {
    int aaa = 1;
  };
namespace A2
  {
    int aaa = 2;
  };

int
foo (void)
{
  int x;

  {
    int block_create;
    using namespace A1;

    block_create = aaa; /* break1 */
  }

  {
    int block_create;
    using namespace A2;

    block_create = aaa; /* break2 */
  }

  return x = 0;
}

