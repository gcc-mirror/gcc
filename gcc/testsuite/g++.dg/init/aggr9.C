// PR c++/53661

enum Code {
  SUCCESS = 0
};

Code a;

int r[] = {a};
