// Build don't link: 
// GROUPS passed templates
template <class ET>
class ChainElem {
public:
  
  ET data;
};

template <class ET>
class Chain {
public:

  ChainElem<ET> *first;

  virtual ~Chain() {}

};

struct B {
};

struct X : B {
  ~X ();
};

struct TBNFexpression {
};

struct TCaseLabelPair {
};

struct TVariant {  
  Chain<TCaseLabelPair> CaseLabelList;
};

struct TCaseConstruct {
  Chain<TBNFexpression> TagFieldPrefixes;
  Chain<TVariant> Variants;
};

struct Production {
  X TypeName;
};

struct SimpleSyntax {
  Chain<Production> Productions;
};
