// Build don't link:
// Origin: Scott Snyder <snyder@fnal.gov> via PR 1733.
// Special g++ Options: -O1
//
// crash test

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
