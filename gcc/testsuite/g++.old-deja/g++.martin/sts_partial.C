// { dg-do run  }
// ecgs-bugs 1999-02-22 14:26 Stefan Schwarzer
// sts@ica1.uni-stuttgart.de
// partial ordering problem in egcs <= 1.1.1

template<class T>
int f(T &){ return 1; }

template<class T>
int f( T[] ){ return 0; }

int main(){
  int d[] ={2};
  return f(d);
}
