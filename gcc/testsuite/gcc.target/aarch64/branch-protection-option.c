/* { dg-do "compile" } */
/* { dg-options "-mbranch-protection=leaf -mbranch-protection=none+pac-ret" } */

/* { dg-error "unexpected 'pac-ret' after 'none'"  "" { target *-*-* } 0 } */
