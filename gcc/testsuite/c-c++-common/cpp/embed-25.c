/* { dg-do preprocess } */
/* { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir --embed-dir=${srcdir}/c-c++-common/cpp/embed-dir/nonexistent" } */
/* { dg-additional-options "-std=c23" { target c } } */

#embed "embed-1.inc" prefix (int a = ) suffix (;)
