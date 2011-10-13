// { dg-do compile }
// { dg-require-effective-target vect_float }

float mem[4096];
const int N=1024;

struct XYZ {
    float * mem;
    int n;
    float * x() { return mem;}
    float * y() { return x()+n;}
    float * z() { return y()+n;}
};

inline
void sum(float * x, float * y, float * z, int n) {
    for (int i=0;i!=n; ++i)
      x[i]=y[i]+z[i];
}

void sumS() {
    XYZ xyz; xyz.mem=mem; xyz.n=N;
    sum(xyz.x(),xyz.y(),xyz.z(),xyz.n);
}

// { dg-final { scan-tree-dump-not "run-time aliasing" "vect" } }
// { dg-final { cleanup-tree-dump "vect" } }
