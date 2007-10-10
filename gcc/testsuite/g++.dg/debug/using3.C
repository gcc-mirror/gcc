// PR debug/31899

namespace NS {
  int x = 0;
  int &ref = x;
}

using NS::ref;
