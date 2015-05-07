// { dg-do run }
// { dg-options "-fsanitize=null" }

int
main (void)
{
  int *p = 0;

  int &r1 = *p;
  int &r2 = *p;
  int &r3 = *p;
  int &r4 = *p;
  int &r5 = *p;
}

// { dg-output "reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'" }
