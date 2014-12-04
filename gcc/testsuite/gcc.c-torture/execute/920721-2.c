/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
f(){}
main(){int n=2;double x[n];f();exit(0);}
