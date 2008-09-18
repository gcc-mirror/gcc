// { dg-do compile }
// Make sure column information is correctly shown in error reporting
// { dg-options "-fshow-column" }


void foo ()
{
  cout << "blah"; // { dg-error "3:'cout'" }
}
