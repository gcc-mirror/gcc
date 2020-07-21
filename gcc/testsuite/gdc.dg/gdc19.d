// https://bugzilla.gdcproject.org/show_bug.cgi?id=19
// { dg-do compile }

void test19()
{
   byte b;
   --b = b;
}
