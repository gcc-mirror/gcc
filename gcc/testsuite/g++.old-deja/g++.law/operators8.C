// { dg-do run  }
// GROUPS passed operators
#include <stdio.h>

class shape {
  public:
    virtual int vDisplay(void) const = 0;
  protected:
    int X;
    int Y;
};

class square :public shape {
  public:
    square(int x, int y, int width_) {
	X = x;
	Y = y;
	width = width_;
    }
    int vDisplay(void) const {
	printf ("PASS\n");
	return 0;
    }
  protected:
    int width;
};


class triangle :public shape {
  public:
    triangle(int x, int y, int width_, int height_) {
	X = x;
	Y = y;
	width = width_;
	height = height_;
    }
    int vDisplay(void) const {
	printf ("FAIL\n");
	return 1;
    }
  protected:
    int width;
    int height;
};

int main() {
    shape* s1 = new square(4,4,5);
    shape* s2 = new triangle(6,6,2,3);
    *s1 = *s2;
    return s1->vDisplay();
}

