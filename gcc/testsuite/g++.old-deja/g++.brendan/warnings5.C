// { dg-do assemble  }
// GROUPS passed warnings
enum TypeKind { ATK, BTK } ;

struct Type {
  enum TypeKind kind : 8;
  void setBTK();
};

void Type::setBTK() { kind = BTK; }
