/* On Darwin, the stub for simple_cst_equal was not being emitted at all 
   causing the as to die and not create an object file.  */

int
attribute_list_contained ()
{
  return (simple_cst_equal ());
}
int
simple_cst_list_equal ()
{
  return (simple_cst_equal ());
}


int __attribute__((noinline))
simple_cst_equal ()
{
  return simple_cst_list_equal ();
}

