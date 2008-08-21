/* { dg-do compile } */
/* { dg-options "-fshow-column -fms-extensions -pedantic" } */

struct { struct a { int x; }; int bar; } hot; /* { dg-warning "29:ISO C doesn't support unnamed" } */
