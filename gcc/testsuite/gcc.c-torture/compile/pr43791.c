int owner();
int clear();

static void fixup() {
   clear();
}

inline __attribute__ ((always_inline))
void slowtrylock(void) {
     if (owner())
         fixup();
}

void fasttrylock(void (*slowfn)()) {
     slowfn();
}

void trylock(void) {
     fasttrylock(slowtrylock);
}

/* { dg-prune-output "(inlining failed in call to always_inline.*indirect function call with a yet undetermined callee|called from here)" } */
