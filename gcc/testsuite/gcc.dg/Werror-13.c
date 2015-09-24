/* { dg-do compile } */
/* { dg-options "-Werror=error -Werror=p, -Werror=l, -Werror=fatal-errors" } */
/* { dg-error "-Wp, is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "-Wl, is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "-Werror is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "-Wfatal-errors is not an option that controls warnings" "" { target *-*-* } 0 } */

int i;
