/* Copyright 2003 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wunknown-pragmas" } */

/* Make sure we get warnings in the expected lines.  */

#pragma unknown1 /* { dg-warning "unknown1" "unknown1" } */

#define COMMA ,
#define FOO(x) x
#define BAR(x) _Pragma("unknown_before") x
#define BAZ(x) x _Pragma("unknown_after")

int _Pragma("unknown2") bar1; /* { dg-warning "unknown2" "unknown2" } */

FOO(int _Pragma("unknown3") bar2); /* { dg-warning "unknown3" "unknown3" } */

int BAR(bar3); /* { dg-warning "unknown_before" "unknown_before 1" } */

BAR(int bar4); /* { dg-warning "unknown_before" "unknown_before 2" } */

int BAZ(bar5); /* { dg-warning "unknown_after" "unknown_after 1" } */

int BAZ(bar6;) /* { dg-warning "unknown_after" "unknown_after 2" } */

FOO(int bar7; _Pragma("unknown4")) /* { dg-warning "unknown4" "unknown4" } */

#pragma unknown5 /* { dg-warning "unknown5" "unknown5" } */
