// { dg-do assemble  }
// GROUPS passed old-abort
class A {};

class SimQuery
{
public:
  SimQuery();
  ~SimQuery();
  int SetMeshFile(char name[]);
protected:
  A& scaling;
  A* mesh;
};

SimQuery::SimQuery():scaling(A) {}// { dg-error "" } .*

SimQuery::~SimQuery() {}

int SimQuery::SetMeshFile(char name[])
{
  mesh = new C;// { dg-error "" } .*
  return 0; // needed to avoid warning of reaching end of non-void fn
}
