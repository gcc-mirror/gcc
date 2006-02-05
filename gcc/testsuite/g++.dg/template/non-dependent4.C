int temp(const char *temp);

template <int> int g() { return temp("Hi"); }
int g1() { return temp("Hi"); }
