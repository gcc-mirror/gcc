// PR c++/53563

template<class T>
struct s
{
 template<class U>
 s(){}
};

int main() {
 struct s<void>::s<int> a;	// { dg-error "no match" }
}
