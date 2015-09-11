/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

class Base {
  public:
     virtual void Primitive ();

};

void Base::Primitive () {

}

void Dispatch (Base * B) {
  B->Primitive ();
}

/* { dg-final { scan-ada-spec-not "CPP_Constructor" } } */
/* { dg-final { cleanup-ada-spec } } */
