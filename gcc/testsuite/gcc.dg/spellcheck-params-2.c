/* { dg-do compile } */
/* { dg-options "--param does-not-resemble-anything=42" } */
/* { dg-error "invalid --param name .does-not-resemble-anything."  "" { target *-*-* } 0 } */

