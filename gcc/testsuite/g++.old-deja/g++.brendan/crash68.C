// { dg-do assemble  }
// { dg-options "-w -O" }
// GROUPS passed old-abort
class   RWDlist
{
public:
  RWDlist& operator=(const RWDlist&);
};
class DataItemRWGDlist : public RWDlist {};

class Base {};
class DataItemList : public Base
{
  private:
  DataItemRWGDlist m_diList;
};      

class StatementGroup
{
  public:
    DataItemList   dataItemList;
    StatementGroup(const StatementGroup&);
};

StatementGroup::StatementGroup(const StatementGroup& sg)
{
  dataItemList = sg.dataItemList;
}
