/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
void x(int a){struct{int p[a],i;}l;l.i;}
