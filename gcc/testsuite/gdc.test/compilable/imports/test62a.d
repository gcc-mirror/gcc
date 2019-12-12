module imports.test62a;

import test62;

struct T()
{
   struct Nested
   {
      S member;
   }
}

alias T!() instance;


