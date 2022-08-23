/* { dg-do "compile" } */

void __attribute__ ((target("branch-protection=leaf")))
foo1 ()
{
}
/* { dg-error {invalid protection type 'leaf' in 'target\("branch-protection="\)' pragma or attribute} "" { target *-*-* } 5 } */
/* { dg-error {pragma or attribute 'target\("branch-protection=leaf"\)' is not valid} "" { target *-*-* } 5 } */

void __attribute__ ((target("branch-protection=none+pac-ret")))
foo2 ()
{
}
/* { dg-error "unexpected 'pac-ret' after 'none'" "" { target *-*-* } 12 } */
/* { dg-error {pragma or attribute 'target\("branch-protection=none\+pac-ret"\)' is not valid} "" { target *-*-* } 12 } */

void __attribute__ ((target("branch-protection=")))
foo3 ()
{
}
/* { dg-error {missing argument to 'target\("branch-protection="\)' pragma or attribute} "" { target *-*-* } 19 } */
/* { dg-error {pragma or attribute 'target\("branch-protection="\)' is not valid} "" { target *-*-* } 19 } */
