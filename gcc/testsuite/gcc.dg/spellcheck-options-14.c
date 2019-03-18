/* Verify that we offer suggestions for misspelled sanitizer options
   (PR driver/78877).  */

/* { dg-do compile } */
/* { dg-options "-fsanitize=addres,nul,this-is-not-a-sanitizer-option" } */
/* { dg-error "unrecognized argument to '-fsanitize=' option: 'addres'; did you mean 'address'" "" { target *-*-* } 0 } */
/* { dg-error "unrecognized argument to '-fsanitize=' option: 'nul'; did you mean 'null'" "" { target *-*-* } 0 } */
/* { dg-error "unrecognized argument to '-fsanitize=' option: 'this-is-not-a-sanitizer-option'" "" { target *-*-* } 0 } */
