// ecgs-bugs 1999-02-22 14:21, Stefan Schwarzer 
// sts@ica1.uni-stuttgart.de
// this code should compile quietly

class CArray 
{
public:
  operator double* (){ return a; }
  // works if we comment this line:
  operator double* () const { return const_cast<double *>(a); }
private:   
  double      a[2];   
};

int main(){
  CArray  a;
  double *pa = a + 1; // gets bogus error - should convert
  return 0; 
}
