    template <class T>
    struct S
    {
      struct R 
      {
	R();
	~R();
      };

      void foo()
      {
	R r;
	int i;
      }

      S();
      ~S();
    };

    void f()
    {
      S<int> si;
      si.foo();
    }
