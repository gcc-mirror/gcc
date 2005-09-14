/* PR c++/23180.  */
/* Initialize a global variable with an expression that attempts to use
   pointer arithmetic to calculate a structure field offset.  */

struct Track {
  char soundName[15];
};

struct SaveLoadEntry {
  int offs;
  int type;
  int size;
};    

int foobar = ((long) (& ((Track *) 42)->soundName[0])) - 42;
