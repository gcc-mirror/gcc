/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long unsigned int size_t;

template<class X>
class A {
public:
        typedef size_t tySize;
        inline void ResizeFast(const tySize & nSize)  {
                if((nSize > m_nAllocSize) && (nSize > 0))   {
                        m_nAllocSize = nSize;
        }
        }
        inline void ResizeFast(const int & nSize) {
        ResizeFast((tySize) nSize);
        }
        tySize m_nAllocSize;
};

class B {
public:
        B *GetNext(void) { };
};

class C  {
public:
        inline void Resize(void) {
                array.ResizeFast(0);
        }
        A<int> array;
};

class D {
private:
        void Do(B * pB, C * pC);
};

void D::Do(B * pB, C * pC)
{
        pC->Resize();
        B * pL = 0;
        for(pL = pB;
                pL != 0;
                pL = pL->GetNext());
}

