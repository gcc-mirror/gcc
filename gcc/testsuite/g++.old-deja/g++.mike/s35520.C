// { dg-do assemble  }

static void kbdNormal() { }
void (*keyHandler)() = kbdNormal;
