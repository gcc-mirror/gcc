extern void abort(void);

typedef int word __attribute__((mode(word)));

struct foo
{
  word x;
  word y[0];
};

int main()
{
  if (sizeof(word) != sizeof(struct foo))
    abort();
  if (__alignof__(word) != __alignof__(struct foo))
    abort();
  return 0;
}
