// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// Don't compile this one with -Wno-long-long...

short volatile short var_0_2;              /* { dg-error "" } duplicate short */
long volatile long var_0_3;                /* { dg-error "" } duplicate long */
signed volatile signed var_0_7;            /* { dg-error "" } duplicate signed */
unsigned volatile unsigned var_0_8;        /* { dg-error "" } duplicate unsigned */
