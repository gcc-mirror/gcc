// { dg-do assemble  }
// The reference parameter to fred isn't dereferenced properly.

class Gump {};
Gump  operator &  (const Gump x){return x;}

class B;

void *sam(int &x)
{return &x;}

const void *fred(const B& x)
{return &x;}  // "&x" causes the compilation error.

class B {};
