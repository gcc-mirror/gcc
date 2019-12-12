/* { dg-do compile } */
/* { dg-options "--param max-early-inliner-iteration=3" } */
/* { dg-error "unrecognized command-line option '--param=max-early-inliner-iteration=3'; did you mean '--param=max-early-inliner-iterations='?"  "" { target *-*-* } 0 } */

