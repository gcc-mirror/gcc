/* Later versions of Darwin can compile for 10.5, but cannot link it so we
   can only run this test up to 10.13.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-do run { target *-*-darwin[89]* *-*-darwin1[0-7]* } } */
/* { dg-skip-if "NeXT only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "ABI 2 only" { *-*-* && { ! objc2 } } { "*" } { "" } } */
/* { dg-additional-options "-fobjc-nilcheck -mmacosx-version-min=10.5 " } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include "pr101666.inc"
