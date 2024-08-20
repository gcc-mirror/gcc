// { dg-do compile { target c++11 } }

int a [[gnu::common]] [2];
int b[2] [[gnu::common]];	// { dg-warning "'common' attribute does not apply to types" }
