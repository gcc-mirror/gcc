// PR c++/33620

template <typename T>
struct __attribute__((visibility("default"))) List {};

int bar(List<int> args);
bool test(const List<int> &);

int i = bar(List<int>());

bool test(const List<int> &) { return true; }
