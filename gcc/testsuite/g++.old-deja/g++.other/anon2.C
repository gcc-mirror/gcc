// { dg-do run  }
extern "C" void abort (void);

static union { 
  int x1; 
  long x2; 
  short x3;
  long x4;
};

static union {
  union {
    union {
      int z;
    };
  };
  union {
    union {
      double d;
      int i;
    };
  };
};


int main()
{
  z = 3;
  if (i != 3)
    abort ();
  d = 2.5;
}
