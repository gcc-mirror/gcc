// { dg-do assemble  }
// GROUPS passed initialization
struct String {
  char * string;
  String(const char* st);  
};

extern char array [];
static String sub = array;
