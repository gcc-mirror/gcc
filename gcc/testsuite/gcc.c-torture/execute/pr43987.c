char B[256 * sizeof(void *)];
typedef void *FILE;
typedef struct globals {
    int c;
    FILE *l;
} __attribute__((may_alias)) T;
void add_input_file(FILE *file)
{
  (*(T*)&B).l[0] = file;
}
extern void abort (void);
int main()
{
  FILE x;
  (*(T*)&B).l = &x;
  add_input_file ((void *)-1);
  if ((*(T*)&B).l[0] != (void *)-1)
    abort ();
  return 0;
}
