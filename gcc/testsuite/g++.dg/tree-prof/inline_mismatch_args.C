/* { dg-options "-O2 -fdump-tree-einline2" } */
class DocId {
 public:
 DocId() { }
 DocId(const DocId &other) {  }
};

int g;
class Base {
 public:
 virtual void Foo(DocId id) { g++; }
};

class Super: public Base {
 public:
 void Foo(DocId id) { }
 void Bar(Base *base, DocId id) __attribute__((noinline));
};

void Super::Bar(Base *base, DocId id) {
 Super::Foo(id); // direct call is inlined
 base->Foo(id); // indirect call is marked do not inline
}

int main(void)
{
 Base bah;
 Super baz;
 DocId gid;

 baz.Bar(&baz, gid);
 return 0;
}
/* { dg-final-use { scan-tree-dump "Inlining virtual void Super::Foo" "einline2"} } */                                                                                
/* { dg-final-use { scan-tree-dump-not "mismatched arguments" "einline2"} } */                                                                 
/* { dg-final-use { cleanup-tree-dump "einline2" } } */
