// PR c++/17852

class T {
#pragma X
  struct S {
  };
#pragma Y
};
