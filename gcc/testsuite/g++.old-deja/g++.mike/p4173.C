// { dg-do assemble  }
// { dg-options "-Wall -ansi -pedantic-errors" }
// This error happens because lvalue is not done well in the C++ front-end.
// NOPs should be lvalues if their arguments are.
// NON_LVALUE_EXPRs shouldn't be.

// prms-id: 4173

enum TypeKind {
    RecordTypeKind
};
struct Type
{
    enum TypeKind kind : 8;
    unsigned char prefixLen;  
};

Type a;
Type b;
TypeKind c;

int
main() {
  a.kind = b.kind = c;
  (a.kind = c) = b.kind;	// { dg-bogus "" } 
}
