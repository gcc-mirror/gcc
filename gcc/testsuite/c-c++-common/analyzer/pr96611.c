struct s { int a; } *ptr;
void unknown_int_ptr (int *);
void unknown_void (void);

void test_1 ()
{
  unknown_int_ptr (&ptr->a);
}

void test_2 ()
{
  unknown_void ();
  unknown_int_ptr (&ptr->a);
}
