

typedef struct
{
  char str[20];
}STACK;
STACK stack[15];
int level;
rezero ()
{
  level = 0;
  __builtin_strcpy (stack[level].str, "");
}


