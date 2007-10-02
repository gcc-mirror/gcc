typedef float V8SF __attribute__ ((vector_size (32)));
void bar (V8SF);
void
foo (float x)
{
  bar ((V8SF) { x, x, x, x, x, x, x, x });
}
