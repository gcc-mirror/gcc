/* { dg-do run } */
/* { dg-skip-if "NeXT only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "ABI 2 only" { *-*-* && { ! objc2 } } { "*" } { "" } } */
/* { dg-additional-options "-fobjc-nilcheck -Wno-objc-root-class" } */

#include "pr101666.inc"

