/* { dg-do "compile" } */
/* { dg-options "-mbranch-protection=leaf -mbranch-protection=none+pac-ret" } */

/* { dg-error "argument 'none' can only appear alone in '-mbranch-protection='" "" { target *-*-* } 0 } */
