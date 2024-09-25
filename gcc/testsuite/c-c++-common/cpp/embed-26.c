/* { dg-do preprocess } */
/* { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir --embed-dir=${srcdir}/c-c++-common/cpp/embed-dir/nonexistent -Wmissing-include-dirs" } */
/* { dg-additional-options "-std=c23" { target c } } */

#embed "embed-1.inc" prefix (int a = ) suffix (;)

/* { dg-warning "No such file or directory.*Wmissing-include-dirs" "-Wmissing-include-dirs" { target *-*-* } 0 } */
