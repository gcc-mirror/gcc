// Build don't link: 
// GROUPS passed enums
enum color { red, green, blue, orange, brown };

struct s {
      enum color      field:2; // ERROR - too small
};
