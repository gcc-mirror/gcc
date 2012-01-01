// PR c++/16603

char const c = 'q';

enum
  {
    x = c,
    y = sizeof(x)
  };

int test[y == sizeof(char) ? 1 : -1];
