int temp(char *temp);

template <int> int g() { return temp("Hi"); }
int g1() { return temp("Hi"); }
