// PR c++/34918
// { dg-do compile }

int v __attribute ((vector_size (8)));
bool b = !(v - v);	// { dg-error "not convert .__vector.2. int. to .bool. in initialization" }
