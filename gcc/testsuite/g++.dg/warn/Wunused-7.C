// PR c++/14481
// { dg-options "-Wunused" }

void func()
{
  struct mybitfields {
    unsigned int s_field:8;
  };
  struct mybitfields s;	// { dg-warning "set but not used" }
  s.s_field = 255;
};

