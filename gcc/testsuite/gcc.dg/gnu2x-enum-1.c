/* Test C2x enumerations with fixed underlying type together with GNU
   extensions: an enum cannot be forward declared without a fixed underlying
   type and then declared or defined with one.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x" } */

enum e1;
enum e1 : int; /* { dg-error "'enum' declared both with and without fixed underlying type" } */

enum e2;
enum e2 : long { A }; /* { dg-error "'enum' declared both with and without fixed underlying type" } */
