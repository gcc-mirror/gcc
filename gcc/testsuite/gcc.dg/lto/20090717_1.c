struct variable {
    const char *string;
};
extern struct variable table[];
int main(int argc, char *argv[]) 
{
  struct variable *p;
  for(p = table; p->string; p++) 
    ;
  return 0;
}
