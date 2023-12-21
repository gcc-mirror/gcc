/* PR sanitizer/113092 */
/* { dg-do run { target bitint575 } } */
/* { dg-options "-fsanitize=shift -fsanitize-recover=shift" } */

int
main ()
{
  volatile _BitInt(255) bi = 12984732985743985734598574358943wb;
  bi = 0 >> bi;
  bi = 329847329847239847239847329847239857489657986759867549867594875984375wb;
  bi = 0 >> bi;
  bi = -12984732985743985734598574358943wb;
  bi = 0 >> bi;
  bi = -329847329847239847239847329847239857489657986759867549867594875984375wb;
  bi = 0 >> bi;
  return 0;
}

/* { dg-output "shift exponent \[0-9a-fx]* is too large for \[0-9]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent \[0-9a-fx]* is too large for \[0-9]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent \[0-9a-fx-]* is negative\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent \[0-9a-fx-]* is negative" } */
