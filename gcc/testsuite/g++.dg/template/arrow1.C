// PR c++/49118
// { dg-do compile }

template< int n >
struct a {
    a< n+1 >
	operator->()
	{ return a< n+1 >(); }
};

int main() {
    a<0>()->x; // { dg-error "instantiation depth exceeds maximum" }
}

// { dg-prune-output "incomplete type" }
// { dg-prune-output "declaration of" }
// { dg-prune-output "used but never defined" }
