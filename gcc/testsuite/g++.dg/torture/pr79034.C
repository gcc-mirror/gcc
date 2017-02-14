/* { dg-do compile } */

extern "C" {
    float sqrtf(float);
}

class T {
public:
    float floats[1];

    inline float length() const {
	return sqrtf(floats[0]);
    }
};

void destruct(void *);

class Container {

    T Ts[1];

public:
    ~Container() {
	destruct((void *)Ts);
    }

    T& operator[](int n) {
	return Ts[0];
    }
};

void fill(Container&);

void doit()
{
  Container data;
  float max = 10;

  int i, j, k;

  for (i = 0; i < 10; i++) {
      for (j = 1; j < 10; j++) {
	  if (max < 5)
	    break;
	  fill( data);
	  max = data[0].length();
	  for (k = 1; k < j; k++) {
	      max = 5;
	  }
      }
  }
}
