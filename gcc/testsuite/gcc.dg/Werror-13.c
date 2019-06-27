/* { dg-do compile } */
/* { dg-options "-Werror=error -Werror=p, -Werror=l, -Werror=fatal-errors -Werror=vla2 -Wno-error=misleading-indentation2" } */
/* { dg-error "'-Wp,' is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "'-Wl,' is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "'-Werror' is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "'-Wfatal-errors' is not an option that controls warnings" "" { target *-*-* } 0 } */
/* { dg-error "'-Werror=vla2': no option '-Wvla2'; did you mean '-Wvla." "" { target *-*-* } 0 } */
/* { dg-error "'-Wno-error=misleading-indentation2': no option '-Wmisleading-indentation2'; did you mean '-Wmisleading-indentation'" "" { target *-*-* } 0 } */

int i;
