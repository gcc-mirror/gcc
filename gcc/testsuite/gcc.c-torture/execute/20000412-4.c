  void f(int i, int j, int radius, int width, int N)
  {
    const int diff   = i-radius;
    const int lowk   = (diff>0 ? diff : 0 );
    int k;
  
    for(k=lowk; k<= 2; k++){
      int idx = ((k-i+radius)*width-j+radius);
      if (idx < 0)
	abort ();
    }
  
    for(k=lowk; k<= 2; k++);
  }
  
  
  int main(int argc, char **argv)
  {
    int exc_rad=2;
    int N=8;
    int i;
    for(i=1; i<4; i++)
      f(i,1,exc_rad,2*exc_rad + 1, N);
    exit (0);
  } 

