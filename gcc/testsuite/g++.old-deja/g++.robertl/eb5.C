// Build don't link:
enum { a, b };

class Bug {
  int pri:8;
  int flags:24;
public:
  void bug() {
    flags |= a;   // this does not work
  }
};

void dummy(Bug x) { x.bug(); }
