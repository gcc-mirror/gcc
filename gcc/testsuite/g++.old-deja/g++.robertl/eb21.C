#include <vector>

#include <strstream.h>

/*----------------------------------------*/

struct connection_t {
  connection_t() {}
};

vector<connection_t> connections;

/*----------------------------------------*/

int
main() {
  ostrstream str;

  connections.insert(connections.end(), connection_t());

  return 0;
}
