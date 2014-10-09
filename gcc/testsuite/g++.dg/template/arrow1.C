// PR c++/49118
// { dg-do compile }

template< int n >
struct a {
    a< n+1 >
	operator->()
	{ return a< n+1 >(); }
};

int main() {
    a<0>()->x; // { dg-error "depth" }
}

// { dg-prune-output "compilation terminated" }
