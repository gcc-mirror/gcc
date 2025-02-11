/* Copyright 2003 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wunknown-pragmas" } */

/* Make sure we get warnings in the expected lines.  */

#pragma unknown1 /* { dg-warning "-:unknown1" "unknown1" } */

#define COMMA ,
#define FOO(x) x /* { dg-note "in definition of macro 'FOO'" } */

#define BAR1(x) _Pragma("unknown_before1") x  /* { dg-warning "unknown_before1" } */
#define BAR2(x) _Pragma("unknown_before2") x  /* { dg-warning "unknown_before2" } */

#define BAZ1(x) x _Pragma("unknown_after1")  /* { dg-warning "unknown_after1" } */
#define BAZ2(x) x _Pragma("unknown_after2")  /* { dg-warning "unknown_after2" } */

int _Pragma("unknown2") bar1; /* { dg-warning "unknown2" "unknown2" } */

FOO(int _Pragma("unknown3") bar2); /* { dg-warning "unknown3" "unknown3" } */

int BAR1(bar3); /* { dg-note "in expansion of macro 'BAR1'" } */

BAR2(int bar4); /* { dg-note "in expansion of macro 'BAR2'" } */

int BAZ1(bar5); /* { dg-note "in expansion of macro 'BAZ1'" } */

int BAZ2(bar6;) /* { dg-note "in expansion of macro 'BAZ2'" } */

FOO(int bar7; _Pragma("unknown4")) /* { dg-warning "-:unknown4" "unknown4" } */

#pragma unknown5 /* { dg-warning "-:unknown5" "unknown5" } */
