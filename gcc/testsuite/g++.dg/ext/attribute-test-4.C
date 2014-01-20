// { dg-do run }

#define vector __attribute__((vector_size(16)))

extern "C" void abort();

union U {
              float f[4];
              vector float v;
} data;

class Star
{
  public:
        static vector float foo();

	Star() 
	  {
	    data.f[0] = 1.0; data.f[1] = 2.0; data.f[2] = 3.0, data.f[3] = 4.0;
	  }

  private:
	friend vector float fTest();
};

vector float Star::foo()
{
    return data.v;
}

vector float fTest()
{
    vector float vf = Star::foo();
    return vf;
}

int main() {

  U data;
  Star s;


  data.v = fTest();
  for (int i=0 ; i < 4; i++)
     if (data.f[i] != (float)(i+1))
       abort();
  return 0;
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector returned by reference.*" } */
/* { dg-prune-output "changes the ABI" } */
