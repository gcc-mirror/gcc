/* { dg-do preprocess } */
/* { dg-options "" } */

/* Based on PR 16999 */

#ident "this is an ident"

/* { dg-final { scan-file "ident-1.i" "(^|\\n)#ident \"this is an ident\"($|\\n)" } } */
