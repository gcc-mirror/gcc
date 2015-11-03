struct T { T () {}; virtual ~T () {}; int t; };
struct S : virtual public T { int a; void foo (); };
template <typename T>
struct U { U () {}; virtual ~U () {}; T t; };
template <typename T>
struct V : virtual public U<T> { T a; void foo (); };

void
S::foo ()
{
#pragma omp parallel firstprivate (a, t)
  {
    int *q1 = &a;
    int *q2 = &this->a;
    int q3 = a;
    int q4 = this->a;
    int *q5 = &t;
    int *q6 = &this->t;
    int q7 = t;
    int q8 = this->t;
    int q9 = T::t;
    int q10 = this->T::t;
    int &q11 = a;
    int &q12 = this->a;
    int &q13 = t;
    int &q14 = this->t;
    int &q15 = S::a;
    int &q16 = this->S::a;
    int &q17 = T::t;
    int &q18 = this->T::t;
  }
#pragma omp parallel private (a, t)
  {
    a = 7;
    S::a += 9;
    t = 10;
    T::t += 11;
  }
#pragma omp parallel
  {
  #pragma omp sections lastprivate (S::a, T::t)
    {
    #pragma omp section
      {
	S::a = 6;
	T::t = 8;
      }
    }
  }
#pragma omp parallel
  {
  #pragma omp for firstprivate (a, t) lastprivate (a, t)
    for (int i = 0; i < 10; i++)
      {
	int q19 = a + t;
	if (i == 9)
	  {
	    a = i;
	    T::t = i + 2;
	  }
      }
  }
#pragma omp sections lastprivate (a, t)
  {
  #pragma omp section
    {
      a = 5;
      t = 6;
    }
  }
#pragma omp for firstprivate (a, t) lastprivate (a, t)
  for (int i = 0; i < 10; i++)
    {
      int q20 = a + t;
      if (i == 9)
	{
	  a = i;
	  T::t = i + 2;
	}
    }
#pragma omp parallel sections lastprivate (a, t)
  {
  #pragma omp section
    {
      a = 5;
      t = 6;
    }
  }
#pragma omp parallel
  {
    #pragma omp task firstprivate (a, t)
    {
      S::a++;
      t++;
    }
  }
#pragma omp parallel
  {
    #pragma omp taskloop firstprivate (a, t) lastprivate (t)
    for (int i = 0; i < a; i++)
      t++;
  }
#pragma omp taskloop firstprivate (a, t) lastprivate (t)
  for (int i = 0; i < a; i++)
    t++;
  a = 1;
  t = 0;
#pragma omp parallel sections reduction (*: S::a) reduction (+: t)
  {
    {
      a = 1;
      t = 2;
    }
    #pragma omp section
    {
      a = 2;
      t = 3;
    }
    #pragma omp section
    {
      a = 3;
      t = 4;
    }
  }
}

template <typename T>
void
V<T>::foo ()
{
#pragma omp parallel firstprivate (a, U<T>::t)
  {
    int *q1 = &a;
    int *q2 = &this->a;
    int q3 = a;
    int q4 = this->a;
    int *q5 = &(U<T>::t);
    int *q6 = &this->U<T>::t;
    int q7 = U<T>::t;
    int q8 = this->U<T>::t;
    int q9 = U<T>::t;
    int q10 = this->U<T>::t;
    int &q11 = a;
    int &q12 = this->a;
    int &q13 = U<T>::t;
    int &q14 = this->U<T>::t;
    int &q15 = V::a;
    int &q16 = this->V::a;
    int &q17 = U<T>::t;
    int &q18 = this->U<T>::t;
  }
#pragma omp parallel private (a, U<T>::t)
  {
    a = 7;
    V::a += 9;
    U<T>::t = 10;
    U<T>::t += 11;
  }
#pragma omp parallel
  {
  #pragma omp sections lastprivate (V::a, U<T>::t)
    {
    #pragma omp section
      {
	V::a = 6;
	U<T>::t = 8;
      }
    }
  }
#pragma omp parallel
  {
  #pragma omp for firstprivate (a, U<T>::t) lastprivate (a, U<T>::t)
    for (int i = 0; i < 10; i++)
      {
	int q19 = a + U<T>::t;
	if (i == 9)
	  {
	    a = i;
	    U<T>::t = i + 2;
	  }
      }
  }
#pragma omp sections lastprivate (a, U<T>::t)
  {
  #pragma omp section
    {
      a = 5;
      U<T>::t = 6;
    }
  }
#pragma omp for firstprivate (a, U<T>::t) lastprivate (a, U<T>::t)
  for (int i = 0; i < 10; i++)
    {
      int q20 = a + U<T>::t;
      if (i == 9)
	{
	  a = i;
	  U<T>::t = i + 2;
	}
    }
#pragma omp parallel sections lastprivate (a, U<T>::t)
  {
  #pragma omp section
    {
      a = 5;
      U<T>::t = 6;
    }
  }
#pragma omp parallel
  {
    #pragma omp task firstprivate (a, U<T>::t)
    {
      V::a++;
      U<T>::t++;
    }
  }
#pragma omp parallel
  {
    #pragma omp taskloop firstprivate (a, U<T>::t) lastprivate (U<T>::t)
    for (int i = 0; i < a; i++)
      U<T>::t++;
  }
#pragma omp taskloop firstprivate (a, U<T>::t) lastprivate (U<T>::t)
  for (int i = 0; i < a; i++)
    U<T>::t++;
  a = 1;
  U<T>::t = 0;
#pragma omp parallel sections reduction (*: V::a) reduction (+: U<T>::t)
  {
    {
      a = 1;
      U<T>::t = 2;
    }
    #pragma omp section
    {
      a = 2;
      U<T>::t = 3;
    }
    #pragma omp section
    {
      a = 3;
      U<T>::t = 4;
    }
  }
}

void
bar ()
{
  V<int> v;
  v.foo ();
}
