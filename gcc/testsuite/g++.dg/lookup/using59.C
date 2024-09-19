
namespace Y
{
  extern int I;
}

using Y::I; // { dg-message "previous declaration" }
extern int I; // { dg-error "conflicts with a previous" }

extern int J;
extern int J; //  { dg-message "previous declaration" }
extern char J; // { dg-error "conflicting declaration" }
