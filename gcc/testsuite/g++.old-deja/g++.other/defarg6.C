// { dg-do assemble  }
// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

#include <vector>
#include <fstream>

class STACK {
public:
  std::vector<int> data;

  STACK() : data()
    { }
};
