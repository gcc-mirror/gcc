/* { dg-do compile } */
/* { dg-options "-fpreprocessed" } */

const unsigned char c[] = {
#embed "embed-6.c" limit (64)	/* { dg-error "'gnu::base64' parameter required in preprocessed source" } */
};
