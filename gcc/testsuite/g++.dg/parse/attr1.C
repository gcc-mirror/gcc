// Test whether attributes are accepted both immediately after
// struct/union keyword and after the closing brace.
// { dg-do compile }

struct foo
{
  union __attribute__ ((packed))
  {
    int a;
    long b;
  };
  union __attribute__ ((packed)) __attribute__ ((unused))
  {
    int c;
    long d;
  };
};

union __attribute__ ((packed)) bar
{
  int c;
  long d;
};

struct __attribute__ ((packed)) baz
{
  int e;
  long f;
};

struct foo2
{
  union
  {
    int a;
    long b;
  } __attribute__ ((packed));
};

union bar2
{
  int c;
  long d;
} __attribute__ ((packed));

struct baz2
{
  int e;
  long f;
} __attribute__ ((packed));
