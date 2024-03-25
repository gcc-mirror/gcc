/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
/* { dg-additional-options "-std=gnu89" } */
f(){}
main(){int n=2;double x[n];f();exit(0);}
