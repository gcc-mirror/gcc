// { dg-do run  }
int main(int argc, char ** argv) {

  int (&var_field_ref)[] =  * (int (*)[]) new int [42]; 
  int (&fix_field_ref)[1] =  * (int (*)[1]) new int [42]; 

  int static_field[42];

  int *const &var_field_ptr_ref  = var_field_ref; // { dg-bogus "" } 
  int *const &fix_field_ptr_ref  = fix_field_ref;
  int *const &static_field_ptr_ref  = static_field;

  int * var_field_ptr  = var_field_ref; // { dg-bogus "" } 
  int * fix_field_ptr  = fix_field_ref; 
  int * static_field_ptr  = static_field; 

  return 0;
}
