/* Verify that we can generate a suggestion of "--warn-no-abi-tag"
   from c.opt's "Wabi-tag" (PR driver/69265).  */

/* { dg-do compile } */
/* { dg-options "-fwarn-no-abi-tag" } */
/* { dg-error "unrecognized command-line option '-fwarn-no-abi-tag'; did you mean '--warn-no-abi-tag'?"  "" { target *-*-* } 0 } */
