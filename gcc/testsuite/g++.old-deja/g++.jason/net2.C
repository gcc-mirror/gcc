// Test of various ?: problems.

class D
{
public:
  void a();
  void b();
  D(int i):x(i) {}
private:
  int x;
};

void D::a() {++x;}
void D::b() {--x;}

  
int aa=1, bb=0;

int fa() {return 0;}
int fb() {return 2;}

int main(int argc, char* argv[])
{
  typedef int* pi;
  int* p = (argc == 1)? &aa: &bb;
  *p = 0;

  typedef int (*ifptr)();
  ifptr fp = (argc == 1)? fa: fb;
  aa = fp();
  
  D d(0);
  typedef void (D::*dmem)();
  dmem mfp = (argc == 1)? &D::a: &D::b;
  (d.*mfp)();
  return 0;
}
