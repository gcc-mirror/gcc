/* { dg-do compile } */
/* { dg-options "-fpreprocessed -fdirectives-only" } */

const unsigned char c[] = {
#embed "embed-7.c" limit (64)	/* { dg-error "'gnu::base64' parameter required in preprocessed source" } */
};
