// { dg-do assemble  }
  signed char *ptr2 = "hello";  /* { dg-error "" } changing sign */
unsigned char *ptr3 = "hello";  /* { dg-error "" } changing sign */
