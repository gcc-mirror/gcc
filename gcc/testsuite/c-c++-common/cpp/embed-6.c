/* { dg-do preprocess } */
/* { dg-options "-M -nostdinc --embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */

#if __has_embed ("embed-1.c")
#embed "magna-carta.txt" limit(1)
#embed "magna-carta.txt" limit(64)
#embed "magna-carta.txt" prefix(1 +)suffix(+ 42)if_empty()
#elif
#embed "non-existent-file"
#endif
#if 1 || __has_embed ("embed-2.c")
#embed <embed-1.inc>
#embed <embed-4.c>
#embed "embed-3.c"
#endif

/* { dg-final { scan-file embed-6.i "(^|\\n)embed-6.o:" } }
   { dg-final { scan-file embed-6.i "embed-6.c" } }
   { dg-final { scan-file embed-6.i "c-c..-common/cpp/embed-dir/magna-carta.txt" } }
   { dg-final { scan-file embed-6.i "c-c..-common/cpp/embed-dir/embed-1.inc" } }
   { dg-final { scan-file embed-6.i "c-c..-common/cpp/embed-dir/embed-4.c" } }
   { dg-final { scan-file embed-6.i "(\[^/]|cpp/)embed-3.c" } } */
/* { dg-final { scan-file-not embed-6.i "embed-1.c" } }
   { dg-final { scan-file-not embed-6.i "embed-2.c" } } */
