#include <vector>

#include <strstream.h>

using namespace std;

/*----------------------------------------*/

struct connection_t {
  connection_t() {}
};

std::vector<connection_t> connections;

/*----------------------------------------*/

int
main() {
  ostrstream str;

  connections.insert(connections.end(), connection_t());

  return 0;
}
