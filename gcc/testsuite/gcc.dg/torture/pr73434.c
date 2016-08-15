/* { dg-do run } */

typedef struct { unsigned char x; } A;
typedef struct { unsigned char x; } B;

int idx = 0;

A objs[1] = {{0}};

int main()
{
  B *b = (B*)&objs[idx];
  b->x++;
  if (b->x)
    b->x = 0;
  if (b->x)
    __builtin_abort ();
  return 0;
}
