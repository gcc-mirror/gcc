// Build don't link:

static void kbdNormal() { }
void (*keyHandler)() = kbdNormal;
