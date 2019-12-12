/* { dg-require-effective-target label_values } */
/* { dg-skip-if "no support for indirect jumps" { bpf-*-* } } */

x(){if(&&e-&&b<0)x();b:goto*&&b;e:;}
