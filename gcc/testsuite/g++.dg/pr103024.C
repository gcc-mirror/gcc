/* { dg-do compile } */
/* { dg-options "-fnon-call-exceptions -fharden-compares -fsignaling-nans" } */

struct G4ErrorMatrix {
  G4ErrorMatrix(int);
  ~G4ErrorMatrix();
};
double PropagateError_charge;
void PropagateError() {
  G4ErrorMatrix transf(0);
  int field(PropagateError_charge && field);
}
