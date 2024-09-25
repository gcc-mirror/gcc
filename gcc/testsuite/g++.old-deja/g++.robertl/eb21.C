// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
#include <vector>

#include <sstream>

using namespace std;

/*----------------------------------------*/

struct connection_t {
  connection_t() {}
};

std::vector<connection_t> connections;

/*----------------------------------------*/

int
main() {
  ostringstream str;

  connections.insert(connections.end(), connection_t());

  return 0;
}
