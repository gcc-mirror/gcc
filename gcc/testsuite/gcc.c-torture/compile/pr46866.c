extern void *malloc(__SIZE_TYPE__);
typedef struct T T;
struct T {
    void (*destroy)(void *);
};
void destroy(union { void *this; } __attribute__((transparent_union)));
static const typeof(destroy) *_destroy  = (const typeof(destroy)*)destroy;
void destroy(void *this);
static T *create_empty(void)
{
  T *this = malloc(sizeof(*this));
  *this = (typeof(*this)){ _destroy };
  return this;
}
void openssl_crl_load(void)
{
  T *this = create_empty();
  destroy(this);
}
