/* { dg-require-effective-target label_values } */
/* { dg-additional-options "-std=gnu89" } */
main(){__label__ l;void*x(){return&&l;}goto*x();abort();return;l:exit(0);}
