// PR c++/21682

namespace one {
  template<typename T> int bar1(T);
}

using one::bar1;

template<typename T> void bar1(T);

template<typename T> void bar1r(T);

namespace oner {
  template<typename T> int bar1r(T);
}

using oner::bar1r;

namespace two {
  template<typename T, typename U> void bar2(T);
}

using two::bar2;

template<typename T> void bar2(T);

template<typename T> void bar2r(T);

namespace twor {
  template<typename T, typename U> void bar2r(T);
}

using twor::bar2r;

namespace three {
  template<int i> void bar3();
}

using three::bar3;

template<typename T> void bar3();

template<typename T> void bar3r();

namespace threer {
  template<int i> void bar3r();
}

using threer::bar3r;
