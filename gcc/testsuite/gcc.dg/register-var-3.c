/* { dg-options "" } */
/* { dg-do compile } */
register int r0; /* { dg-error "register name not specified" } */
register int bp; /* { dg-error "register name not specified" } */
register int sp; /* { dg-error "register name not specified" } */
register int r30; /* { dg-error "register name not specified" } */
register int toc; /* { dg-error "register name not specified" } */
register int d0; /* { dg-error "register name not specified" } */
register int a0; /* { dg-error "register name not specified" } */
