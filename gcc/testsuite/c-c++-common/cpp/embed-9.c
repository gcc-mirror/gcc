/* { dg-do preprocess } */
/* { dg-options "" } */

#embed <non-existent-file> /* { dg-error "no include path in which to search for non-existent-file" } */
