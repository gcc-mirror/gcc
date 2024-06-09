/* PR c++/111918 */
/* { dg-do compile { target c++11 } } */
/* { dg-options "-Wno-narrowing" } */
float f1{123456789}; /* { dg-bogus "-Wnarrowing" } */
#pragma GCC diagnostic push
#pragma GCC diagnostic warning "-Wnarrowing"
float f2{123456789}; /* { dg-warning "-Wnarrowing" } */
#pragma GCC diagnostic pop
float f3{123456789}; /* { dg-bogus "-Wnarrowing" } */
