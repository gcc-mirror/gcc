/* { dg-do assemble } */
/* { dg-options "-O1" }*/
// This was PR C++/31165
// We used to copy the whole decl when we just wantted to
// unshare some expressions for the default argument.
class string {
  char *ptr;
  int len;
  int sz;
};
class cset { }  _cset_init;
string an_empty_string;
void f(string& = an_empty_string);
void
h (void )
{
f();
}


