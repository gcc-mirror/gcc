extern int global; // { dg-warning "Decl Global global" }
int global_array[] = { 1, 2, 3 }; // { dg-warning "Decl Global global_array" }

int takes_args(int arg1, int arg2)
{
  int local = arg1 + arg2 + global; // { dg-warning "Decl Local local" }
  return local + 1;
}

int global = 12; // { dg-warning "Decl Global global" }

struct test_str {
  int field; // { dg-warning "Decl Field field" }
};

class test_class {
  int class_field1; // { dg-warning "Decl Field class_field1" }
  int class_field2; // { dg-warning "Decl Field class_field2" }

  test_class() // { dg-warning "Decl Function __ct" }
    : class_field1(0), class_field2(0)
  {}

  void swap_fields(int bias) // { dg-warning "Decl Function swap_fields" }
  {
    int temp = class_field1 + bias; // { dg-warning "Decl Local temp" }
    class_field1 = class_field2 - bias;
    class_field2 = temp;
  }
};
