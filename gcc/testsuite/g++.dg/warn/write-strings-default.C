// { dg-do compile }
// Test the default for -Wwrite-strings

int main()
{
   char* p = "Asgaard";         // { dg-warning "deprecated|forbids converting a string constant" }
}
