// { dg-do assemble  }
typedef int Int;
 
Int Int_object_1;
 
void test ()
{
        ((Int) Int_object_1) = Int_object_1; /* { dg-error "" } not an lvalue*/
}
