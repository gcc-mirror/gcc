class C { 
public: 
    explicit C(int) {} 
}; 
 
int main() 
{ 
    int i = 0; 
    static_cast<C>(i); 
    return 0; 
}
