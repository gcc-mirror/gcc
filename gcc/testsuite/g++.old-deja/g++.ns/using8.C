// Build don't link: 
namespace M {
   int i;
}
namespace N {
  using namespace M;
}

using namespace N;
int j = i;

namespace O{
  int k;
}

namespace N {
  using namespace O;
}

int l = k;
