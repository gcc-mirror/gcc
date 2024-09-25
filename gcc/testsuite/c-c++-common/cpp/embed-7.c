/* { dg-do preprocess } */
/* { dg-options "" } */

#embed "non-existent-file" /* { dg-error "non-existent-file: No such file or directory" } */
/* { dg-prune-output "compilation terminated" } */
