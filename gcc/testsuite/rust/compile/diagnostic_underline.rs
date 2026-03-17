// { dg-additional-options "-quiet" }
#![feature(no_core)]
#![no_core]


/* { dg-options "-fdiagnostics-show-caret" } */


fn barbarbar() {}

const fn foo() { 
    barbarbar();// { dg-error "only functions marked as 'const' are allowed to be called from constant contexts" }
/* { dg-begin-multiline-output "" }
     barbarbar();//
     ^~~~~~~~~
{ dg-end-multiline-output "" } */
}

