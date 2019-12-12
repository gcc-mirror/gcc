/* { dg-require-effective-target indirect_calls } */

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
