#define vector __attribute__((vector_size(16) ))
vector float g(void)
{
  float t = 1.0f;
  return (vector float){0.0, 0.0, t, 0.0};
}

