struct Track {
  char soundName[15];
};

struct SaveLoadEntry {
  int offs;
  int type;
  int size;
};    

int foobar = ((long) (& ((Track *) 42)->soundName[0])) - 42;
