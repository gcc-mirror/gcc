/* { dg-do "compile" } */

void __attribute__ ((target("branch-protection=leaf")))
foo1 ()
{
}
/* { dg-error {invalid argument 'leaf' for 'target\("branch-protection="\)'} "" { target *-*-* } 5 } */
/* { dg-error {pragma or attribute 'target\("branch-protection=leaf"\)' is not valid} "" { target *-*-* } 5 } */

void __attribute__ ((target("branch-protection=none+pac-ret")))
foo2 ()
{
}
/* { dg-error {argument 'none' can only appear alone in 'target\("branch-protection="\)'} "" { target *-*-* } 12 } */
/* { dg-error {pragma or attribute 'target\("branch-protection=none\+pac-ret"\)' is not valid} "" { target *-*-* } 12 } */

void __attribute__ ((target("branch-protection=")))
foo3 ()
{
}
/* { dg-error {missing feature or flag for 'target\("branch-protection="\)'} "" { target *-*-* } 19 } */
/* { dg-error {pragma or attribute 'target\("branch-protection="\)' is not valid} "" { target *-*-* } 19 } */
