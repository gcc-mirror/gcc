// Special g++ Options: -O2 -w
// egcs-bugs 1999-02-22 14:24 Stefan Schwarzer
// sts@ica1.uni-stuttgart.de
// optimizer problem in egcs <= 1.1.1

struct XTVec{
  XTVec(){x[0]=x[1] =x[2] =0;}
  XTVec(int ax,int y=0.,int z=0.){x[0]=ax;x[1]=y; x[2]=z; }
  int& operator[](int);

  int x[3];
};

inline 
int & XTVec::operator[](int i){
  return x[i];
}

inline 
XTVec& operator+=(XTVec& lhs, XTVec& rhs){
  lhs[0]+=rhs[0];
  lhs[1]+=rhs[1];
  lhs[2]+=rhs[2];
  return lhs;
}

inline 
XTVec operator+(XTVec& lhs, XTVec& rhs){
  XTVec result(lhs);
  return result += rhs;
}

int main()
{
  XTVec ur(4.,0.,1.);
  XTVec ll(0.,2.,0.);
  XTVec initsum(ur + ll);

  // sum of components should be 7
  return (initsum[0] + initsum[1] + initsum[2] - 7);
}
