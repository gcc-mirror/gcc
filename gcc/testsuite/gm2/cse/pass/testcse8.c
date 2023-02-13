
struct m2string {
  char *contents;
  int HIGH;
};

typedef struct m2string STRING;
static inline void inline StrLen (STRING a) __attribute__ ((always_inline));
static inline void inline foo (void) __attribute__ ((always_inline));

static void StrLen (STRING f)
{
  **((char **)&f) = 'g';
}

static void foo (void)
{
  STRING a;

  a.contents = "hello";
  a.HIGH = 6;
  StrLen(a);
}

void init (void)
{
  foo();
}


