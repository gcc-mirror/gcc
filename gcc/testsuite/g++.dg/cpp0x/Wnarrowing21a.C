/* PR c++/111918 */
/* { dg-do compile { target c++11 } } */
/* { dg-options "" } Suppress default -pedantic-errors so we can test permerror functionality.  */
extern int g ();
float f1{123456789}; /* { dg-error "-Wnarrowing" } */
float f2{g ()}; /* { dg-warning "-Wnarrowing" } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnarrowing"
float f3{123456789}; /* { dg-bogus "-Wnarrowing" } */
float f4{g ()}; /* { dg-bogus "-Wnarrowing" } */
#pragma GCC diagnostic pop
float f5{123456789}; /* { dg-bogus "warning" "warning in place of error" } */
                     /* { dg-error "-Wnarrowing" "" { target *-*-* } .-1 } */
float f6{g ()}; /* { dg-warning "-Wnarrowing" } */
