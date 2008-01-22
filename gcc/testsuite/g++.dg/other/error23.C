// PR c++/34918
// { dg-do compile }

int v __attribute ((vector_size (8)));
bool b = !(v - v);	// { dg-error "could not convert .\\(int __vector__\\)\\{0, 0\\}. to .bool.|in argument to unary" }
