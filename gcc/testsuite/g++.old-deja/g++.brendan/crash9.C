// Build don't link: 
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

SimQuery::SimQuery():scaling(A) {}// ERROR - .*

SimQuery::~SimQuery() {}// ERROR - 

int SimQuery::SetMeshFile(char name[])
{// ERROR - 
  mesh = new C;// ERROR - .*
  return 0; // needed to avoid warning of reaching end of non-void fn
}
