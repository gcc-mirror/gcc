// Build don't link:
// Warn if a enum cannot fit into a small bit-field.

enum TypeKind { ATK, BTK, CTK, DTK } ;

struct Type {
  enum TypeKind kind : 1;		// WARNING - 
  void setBTK();
};

void Type::setBTK() { kind = DTK; }
