// Build don't link: 
int*& foo (int const *& x)
{
  return const_cast<int*&> (x);
}

/*
If the references in this example are changed to pointers (change
all `&''s to `*'), no warnings result.

I think this is incorrect according to CD2 5.2.11, para 4:

4 An  lvalue of type T1 can be explicitly converted to an lvalue of type
  T2 using the cast const_cast<T2&> (where T1 and T2 are  object  types)
  if  a pointer to T1 can be explicitly converted to the type pointer to
  T2 using a const_cast.  The result of a reference const_cast refers to
  the original object.

*/
