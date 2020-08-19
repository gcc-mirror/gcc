// https://bugzilla.gdcproject.org/show_bug.cgi?id=127
// { dg-do compile }

int[0] test127a;     // OK
int[1][0] test127b;  // OK
int[0][1] test127c;  // ICE
