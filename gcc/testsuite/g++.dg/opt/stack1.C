// PR optimization/11198
// Origin: Joerg Walter <jhr.walter@t-online.de>
// Reduced testcase by: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
//                      Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// The compiler used to allocate the same stack slot for two aggregates,
// overlooking that assignments to members given the same address on the
// stack may not alias and thus may be reordered by the scheduling passes.

// { dg-do run }
// { dg-options "-O2 -frename-registers" }


double zero_;

inline const int&
min(const int& a, const int& b) {
  if (b < a) return b; return a;
}

struct barrier { barrier () {} };

template <typename=void> struct unbounded_array {
    inline unbounded_array (): data_ (new double [9]) {}
    inline double& operator [] (int i) { return data_ [i]; }
    double* data_;
};

inline int element (int i, int j) {
  return i + j;
}

template <typename=void>
struct matrix {
    inline matrix () : size2_ (3) {}

    inline unbounded_array<> &data () { return data_; }

    inline double& el (int i, int j) {
      int dead1 = j;
      int dead2 = 1 + i - j;
      if (j < size2_ && i-j < 2)
	return data () [element (j,i-j+1)];
      barrier ();
      return zero_;
    }

    struct iterator2;

    inline iterator2 find () {
      return iterator2 (*this);
    }

    struct iterator1 {
        inline iterator1 (matrix *m):
			dead1 (m), i (0) {}
	void *dead1;
        int i;
        int dead2;
    };

    const int size2_;
    unbounded_array<> data_;
};


template<typename=void>
struct adaptor {
    adaptor (matrix<> &m) : m(&m), upper_ (1) {}

    int size1 () const;
    int size2 () const     { return 3; }
    int lower () const     { return 1; }
    int upper () const     { return upper_; }
    matrix<> &data () { return *m; }

    double& el (int i, int j) {
      int dead1, dead2;
      if (j < size2 () && i-j < 1)
	return data ().el (i, j);

      barrier ();
      return zero_;
    }

    struct a_iterator2;

    struct a_iterator1 {
        a_iterator1 (adaptor &a, const matrix<>::iterator1 &it1):
			a (&a), dead1 (it1) {}

        a_iterator2 begin () const {
	  return a_iterator2(*a);
        }
	adaptor *a;	
        matrix<>::iterator1 dead1;
    };

    struct a_iterator2 {
        a_iterator2 (adaptor &a) : a (&a) {}

        double& f () const {
	  int i = 0;
	  int l = a->upper () + i;
	  int q = a->size2 ();
	  if (0 < q &&
	      l < a->lower () + 1 + a->upper ())
	    return a->m->el(0,0);

	  return a->el (i, 0);
        }

	adaptor *a;
    };

    matrix<> *m;
    int upper_;
};

void matrix_swap (adaptor<> &bam1, adaptor<> &bam2)
{
  adaptor<>::a_iterator1 it1 (bam1,matrix<>::iterator1(bam1.m)),
                         it2 (bam2,matrix<>::iterator1(bam2.m));
  int dead;
  double x = it1.begin().f();
  it2.begin().f() = x;
}

int main ()
{
  matrix<> m1,m2;
  adaptor<> bam1 (m1), bam2 (m2);
  matrix_swap (bam1, bam2);
  return 0;
}
