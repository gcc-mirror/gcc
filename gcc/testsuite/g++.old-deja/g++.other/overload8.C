class a {
public:
  int f()        { return 0; }
  int f() const  { return 1; }
};

class b : public a {
};

int main() 
{
  int (b::* ptr1)()       = &b::f; 
  int (b::* ptr2)() const = &b::f; 
  
  b ao;
  
  if ((ao.*ptr1)() != 0)
    return 1;
  if ((ao.*ptr2)() != 1)
    return 1;
}

