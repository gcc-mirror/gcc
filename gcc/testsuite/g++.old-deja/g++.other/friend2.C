// { dg-do assemble  }

void
f()
{
  class Local_2 {
    friend class Friend;

    int i;
  };
  
  class Friend {
  public:
    void g() {
      Local_2 l2;
      l2.i = 3;
    }
  };
}
