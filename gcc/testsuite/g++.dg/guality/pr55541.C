// PR debug/55541
// { dg-do run }
// { dg-options "-g" }

int
main ()
{
  int vari;
  vari = 10;
  vari = vari + 5;
} // { dg-final { gdb-test 11 "vari" "15" } }
