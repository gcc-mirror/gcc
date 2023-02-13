
struct m2string {
  char *contents;
  int HIGH;
};

typedef struct m2string STRING;
static inline void inline foo (STRING a) __attribute__ ((always_inline));


static void foo (STRING f)
{
  **((char **)&f) = 'g';
}

void init (STRING a)
{
  foo(a);
}


