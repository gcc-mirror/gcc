/* Tests error recovery for invalid code.  */
__attribute__((foo)  int f (){} /* { dg-error "(parse error|syntax error|expected '\\)') before 'int'" } */
