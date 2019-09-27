typedef struct foo {} foo_t;

typedef void (*func_t)(foo_t *s);

void cb_1 (foo_t *s);
void cb_2 (foo_t *s);

typedef struct config_s {
  func_t func;
} config;

static const config table[2] = {
  { cb_1 },
  { cb_2 }
};

int deflate (foo_t *s, int which)
{
  (*(table[which].func))(s);
}
