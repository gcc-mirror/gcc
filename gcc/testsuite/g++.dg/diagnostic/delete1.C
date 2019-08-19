void f ()
{
  int a[1];
  delete (a);  // { dg-warning "11:deleting array" }

  bool b;
  delete (b);  // { dg-error "11:type .bool. argument" }

  void g ();
  delete (g);  // { dg-error "11:cannot delete a function" }

  void* p;
  delete (p);  // { dg-warning "11:deleting .void*." }
}
