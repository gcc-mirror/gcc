// { dg-do compile }
// { dg-options -Wwrite-strings }

int main()
{
   char* p = "Asgaard";         // { dg-warning "deprecated|forbids converting a string constant" }
}
