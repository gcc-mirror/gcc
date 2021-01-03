// PR c++/25814
// { dg-do compile }
// Test from Wikipedia.

class Timer {
 public:
  Timer();
};

class TimeKeeper {
 public:
  TimeKeeper(const Timer& t);

  int get_time();
};

void f(double adouble) {
  int i(int(adouble)); // { dg-warning "parentheses were disambiguated as a function declaration" }
}

int main() {
  TimeKeeper time_keeper(Timer()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  return time_keeper.get_time(); // { dg-error "request for member" }
}
