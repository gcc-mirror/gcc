/* PR target/13889 */
struct { long long a; } *p;
void initNetFlowFunct(void) {
  unsigned int b = (unsigned int)-1;
  p->a = b;
}

