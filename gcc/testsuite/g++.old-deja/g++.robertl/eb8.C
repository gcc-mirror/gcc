
class foo {
public:
  operator <<(const void *);
  operator <<(char *);
};

void main()
{
  foo f;
  f << (void*)0;
}

