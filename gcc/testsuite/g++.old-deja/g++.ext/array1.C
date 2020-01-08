// { dg-do assemble  }
// { dg-options "" }
// Testcase for assignment of non-array to array (assigns the same value to
// each member)
// Special Options: 

typedef struct {} ct2d_rigid, ct2d_rigid_a[1];
class ccInspection
{
protected:
  ct2d_rigid_a _dev2phys;	 
};
class ccBgaInspection : public ccInspection
{
public:
  void reinspect (unsigned long diagFlags);
};
void
ccBgaInspection::reinspect (unsigned long diag_flags) 
{
  ct2d_rigid physTdev;		 
  _dev2phys = physTdev; // { dg-error "13:incompatible types in assignment" } 
}
