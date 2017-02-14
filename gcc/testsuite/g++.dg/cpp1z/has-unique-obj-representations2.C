struct S;
struct T { S t; };					// { dg-error "incomplete type" }
struct U { int u[sizeof (S)]; };			// { dg-error "incomplete type" }
union V { char c; char d[]; };				// { dg-error "flexible array member in union" }
bool a = __has_unique_object_representations (S);	// { dg-error "incomplete type" }
bool b = __has_unique_object_representations (T);
bool c = __has_unique_object_representations (U);
bool d = __has_unique_object_representations (V);
