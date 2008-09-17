// { dg-do assemble  }
// GROUPS passed enums
enum color { red, green, blue, orange, brown };

struct s {
      enum color      field:2; // { dg-warning "too small" }
};
