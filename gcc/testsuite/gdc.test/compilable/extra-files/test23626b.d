interface Timeline {
}

struct Policy {
  alias OldTagCallback = void delegate() @nogc nothrow;
  Timeline timeline;
  OldTagCallback oldTagCB;
}

import test23626;

struct Tiering {
    StaticHashTable!(Policy) policies;
}
