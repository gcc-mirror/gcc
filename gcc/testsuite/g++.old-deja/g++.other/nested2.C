// { dg-do assemble  }
// by Bert Bril <bert@dgb.nl>

struct M1 {
    struct I                            {};
};
 
struct M2 {
    struct I                            {};
    struct J : virtual public M2::I,
               virtual public M1::I     {};
};
