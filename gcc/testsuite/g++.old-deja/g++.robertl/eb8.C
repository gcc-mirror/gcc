// { dg-do assemble  }

class foo {
public:
  operator <<(const void *);  //{ dg-error "" } no return type
  operator <<(char *);        //{ dg-error "" } no return type
};

void main()		      // { dg-error "must return .int" }
{
  foo f;
  f << (void*)0;
}

