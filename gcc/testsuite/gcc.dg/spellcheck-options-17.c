/* { dg-do compile } */
/* { dg-options "-fno-sanitize=threed" } */
/* { dg-error "unrecognized argument to -fno-sanitize= option: .threed.; did you mean .thread." "" { target *-*-* } 0 } */
