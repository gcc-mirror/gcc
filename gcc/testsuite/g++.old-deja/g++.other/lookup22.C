// Origin: GerhardTonn@gmx.de 
// Build don't link:

struct super {
  union {
    int myName;     
    void* secondMember;         
  };
};

struct sub : super {
  int myName() { return 1; }
};
