void test_1 ()
{
  char* p = new char; // { dg-message "allocated here" }
} // { dg-warning "leak of 'p'" }

void test_2 ()
{
  char* p = new char;
  delete p;
}

/* double-delete shows up as use-after-delete
   due to a clobber before the delete.  */

void test_3 ()
{
  char* p = new char;
  delete p; // { dg-message "deleted here" }
  delete p;
} // { dg-warning "use after 'delete'" }
// FIXME: should be on the 2nd delete, not here

void test_4 ()
{
  char *p = new char[16]; // { dg-message "allocated here" }
  delete[] p; // { dg-message "first 'delete\\\[\\\]' here" }
  delete[] p; // { dg-warning "double-'delete\\\[\\\]' of 'p'" }
}

void test_5 ()
{
  char *p = new char[16];
  delete p; // { dg-warning "'p' should have been deallocated with 'delete\\\[\\\]' but was deallocated with 'delete'" }
}

void test_6 ()
{
  char *p = new char;
  delete[] p; // { dg-warning "'p' should have been deallocated with 'delete' but was deallocated with 'delete\\\[\\\]'" }
}

char test_7 (char *p)
{
  delete p;  // { dg-message "deleted here" }
  return *p; // { dg-warning "use after 'delete' of 'p'" }
}

char test_8 (char *p)
{
  delete[] p; // { dg-message "deleted here" }
  return *p;  // { dg-warning "use after 'delete\\\[\\\]' of 'p'" }
}
