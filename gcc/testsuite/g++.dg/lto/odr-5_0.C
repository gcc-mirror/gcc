// { dg-lto-do link }
struct wiimote_t { // { dg-lto-message "type" 2 }
  // Here we get two warnings:
  // warning: type 'struct wiimote_t' violates the C++ One Definition Rule
  // note: type 'const int' should match type 'int'
  const int unid; // { dg-lto-message "the first difference of corresponding definitions is field 'unid'" 1 }
} * a; 

int
main()
{
}
