// { dg-do assemble  }
// Origin: GerhardTonn@gmx.de 

struct super {
  union {
    int myName;     
    void* secondMember;         
  };
};

struct sub : super {
  int myName() { return 1; }
};
