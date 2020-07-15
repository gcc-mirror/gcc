// PR c++/96179
// { dg-do compile { target c++11 } }

template<typename T> struct vector
{
  void push_back(T) { }
};

struct dummy{
        int a;
};

void Modify_Dummy(dummy &d){
        d.a=1;
}

template <bool bla=true> void Templated_Function(){
        vector<dummy> A;
        A.push_back(Modify_Dummy(dummy{0})); // { dg-error "cannot bind non-const lvalue reference" }
}

int main(){
        Templated_Function();
}
