// { dg-do run }

#define vector __attribute__((vector_size(16)))

extern "C" void abort();

class Star
{
  public:
        inline vector float foo() const;

	Star() 
	  {
	    data.f[0] = 1.0; data.f[1] = 2.0; data.f[2] = 3.0, data.f[3] = 4.0;
	  }

  private:
         union {
              float f[4];
              vector float v;
         } data;

	friend vector float fTest(const Star &);
};

vector float Star::foo() const  // { dg-warning "vector returned by ref" "" { target { powerpc*-*-linux* && ilp32 } } }
{
    return data.v;
}

vector float fTest(const Star & val)
{
    vector float vf = val.foo();
    return vf;
}

int main() {

  Star s;

  union u {
              float f[4];
              vector float v;
  } data;

  data.v = fTest(s);
  for (int i=0 ; i < 4; i++)
     if (data.f[i] != (float)(i+1))
       abort();
  return 0;
}


  

