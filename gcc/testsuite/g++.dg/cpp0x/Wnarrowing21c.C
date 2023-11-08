/* PR c++/111918 */
/* { dg-do compile { target c++11 } } */
/* { dg-options "-fpermissive" } */
float f1{123456789}; /* { dg-warning "-Wnarrowing" } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnarrowing"
float f2{123456789}; /* { dg-bogus "-Wnarrowing" } */
#pragma GCC diagnostic pop
float f3{123456789}; /* { dg-warning "-Wnarrowing" } */
