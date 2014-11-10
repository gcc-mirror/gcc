/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
x(a){struct{int p[a],i;}l;l.i;}
