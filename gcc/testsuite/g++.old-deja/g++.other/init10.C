int i;

struct D {
  D () {
    i++;
  }
};

struct C {
  C() {}
    
  D d[1];
};


int main ()
{
  C c;

  if (i != 1)
    return 1;
}
