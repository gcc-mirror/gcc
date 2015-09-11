// PR c++/65513
// { dg-do compile { target c++11 } }

template <typename _Tp> struct atomic {
  atomic() = default;
  atomic(_Tp);
};

struct {
  atomic<bool> bReadyToFlush;
}

LogThreadsleLogEntries[10]{};
