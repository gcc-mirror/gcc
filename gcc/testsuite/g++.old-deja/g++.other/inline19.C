// { dg-do assemble  }
// { dg-options "-O1" }
// Origin: Scott Snyder <snyder@fnal.gov> via PR 1733.
//

struct TBtItem
{
   TBtItem();
};


struct TBtInnerNode
{
   TBtInnerNode();
   int     MaxIndex() const { return 10; }
};


TBtInnerNode::TBtInnerNode()
{
   new TBtItem[MaxIndex()+1];
}
