/* { dg-do compile } */
/* { dg-skip-if "-flto not supported" { { hppa*-*-hpux* } && { ! lp64 } } } */
/* { dg-options "-flto=sparta" } */
/* { dg-error "unrecognized argument to '-flto=' option: 'sparta'" "" { target *-*-* } 0 } */
