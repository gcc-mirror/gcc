// 981203 bkoz
// g++/15822

#include <assert.h>

static unsigned int gcount;

struct playahermosa {
  playahermosa() { ++gcount; }
  playahermosa(const playahermosa &) { ++gcount; }
  ~playahermosa() { --gcount; }
};

struct playacoco {
  playacoco(const playahermosa& = playahermosa()) {  } //create a temporary
};

void foo(playacoco *) { }

int main() 
{
   playacoco bar[2];
   foo(bar);
   assert (gcount == 0);

   return 0;
}
