typedef int Int;
 
Int Int_object_1;
 
void test ()
{
        ((Int) Int_object_1) = Int_object_1; /* ERROR - not an lvalue*/
}
