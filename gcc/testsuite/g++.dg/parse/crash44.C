// PR c++/37260
// { dg-options "" }

struct pthread_once_t { };
struct test {
  pthread_once_t once;
};

int main(void) {
  struct test foo = {
    once: PTHREAD_ONCE_INITIALIZER // { dg-error "'PTHREAD_ONCE_INITIALIZER' was not declared in this scope" }
  };

  return 0;
}
