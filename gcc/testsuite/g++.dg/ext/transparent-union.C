// PR c++/53761

typedef union {    // { dg-error "type transparent" }
   double x;
} __attribute__(( __transparent_union__ )) example_t;
