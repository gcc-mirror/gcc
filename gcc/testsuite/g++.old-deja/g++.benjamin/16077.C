// { dg-do assemble  }
// 981203 bkoz
// g++/16077
// { dg-options "-Wconversion" }

class nicaragua;
struct colombia {
   colombia();
   colombia(const colombia &);
   colombia(const nicaragua &);
   colombia &operator= (const colombia&);
};

struct nicaragua {
public:
   nicaragua();
   nicaragua(const nicaragua&);
   operator colombia();
};

void peace(const colombia&);

void foo(nicaragua& b) {
  peace(b); // { dg-warning "" } // WARNING -
}




