// Produces ICE 980519.
// Test case from Dirk Engelmann <Dirk.Engelmann@IWR.Uni-Heidelberg.De>

namespace vector {

  // allocate memory for vector
        
        template <class T>
        inline T* alloc(const int aWidth)
        {
                // allocate memory
                return new T[aWidth];
        }

}

namespace matrix {

  // allocate memory for matrix
        template <class T>
        T** alloc(const int aWidth,const int aHeight)
        {
                // allocate memory
                T **mat = vector::alloc<T*>(aHeight);
                T *data = vector::alloc<T> (aWidth*aHeight);
                // set pointer
                for (int i=0; i<aHeight; i++)
                        mat[i] = &data[aWidth*i];
                // ok
                return mat;
        }

}

int main(void)
{
  // sample
  double **m=matrix::alloc<double>(10,20);

}
