// PR c++/17916

class T {
#pragma X
  struct S {
  };
#pragma Y
};
