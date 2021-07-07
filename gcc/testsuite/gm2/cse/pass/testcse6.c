
struct m2string {
  char *contents;
  int HIGH;
};

typedef struct m2string STRING;

void init (STRING a)
{
  **((char **)&a) = 'g';
}


