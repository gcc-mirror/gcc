
class foo {
public:
  operator <<(const void *);  //ERROR - no return type
  operator <<(char *);        //ERROR - no return type
};

void main()
{                             //ERROR - wrong return type for main
  foo f;
  f << (void*)0;
}

