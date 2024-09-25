// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

#include <vector>
#include <fstream>

class STACK {
public:
  std::vector<int> data;

  STACK() : data()
    { }
};
