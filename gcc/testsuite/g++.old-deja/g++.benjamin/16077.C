// 981203 bkoz
// g++/16077
// Build don't link: 

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

void peace(const colombia&); // WARNING - // WARNING -

void foo(nicaragua& b) {
  peace(b); // WARNING - // WARNING -
}




