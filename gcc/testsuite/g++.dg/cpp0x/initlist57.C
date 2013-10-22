// PR c++/50054
// { dg-options -std=c++11 }

void g( const int& (a)[1] ) {}	// { dg-error "array of references" }

int main () {
    g( { 1, 2 } );		// { dg-error "initializer list" }
}
