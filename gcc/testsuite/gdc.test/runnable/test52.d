// PERMUTE_ARGS:

// 2311

extern(C)
{
    void exit(int);
    int printf(const char*, ...);
}

struct X()
{
  static this()
  {
     printf("this()\n");
  }
  static ~this()
  {
     printf("~this()\n");
     exit(0);
  }
}

static ~this()
{
     printf("g: ~this()\n");
}

int main() { alias X!() x; return 1; }
