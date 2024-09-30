/* { dg-do preprocess } */
/* { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */

#embed <non-existent-file> /* { dg-error "non-existent-file: No such file or directory" } */
/* { dg-prune-output "compilation terminated" } */
