// 981204 bkoz
// g++/17922
// Build don't link:

class base { };

struct derived : public base   {
   derived (const derived&);
   derived (const base&);
};

class tahiti {
public: 
   static void mf (derived);
};

void foo (const derived aaa) {
   tahiti::mf(aaa);
}
