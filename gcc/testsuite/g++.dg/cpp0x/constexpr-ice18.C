// PR c++/67831
// { dg-do compile { target c++11 } }

struct Task {
  struct TaskStaticData {
    constexpr TaskStaticData() {}
  } const &tsd;
  constexpr Task() : tsd(TaskStaticData()) {}
};

Task tasks{Task()};
