// Special g++ Options: -O2

/* KAI's version of Stepanov Benchmark -- Version 1.2

   Version 1.2 -- removed some special code for GNU systems that
		  GNU complained about without -O

To verify how efficiently C++ (and in particular STL) is compiled by
the present day compilers, I composed a little benchmark. It outputs
13 numbers. In the ideal world these numbers should be the same. In
the real world, however, ...

The final number printed by the benchmark is a geometric mean of the
performance degradation factors of individual tests. It claims to
represent the factor by which you will be punished by your
compiler if you attempt to use C++ data abstraction features. I call
this number "Abstraction Penalty."

As with any benchmark it is hard to prove such a claim; some people
told me that it does not represent typical C++ usage. It is, however,
a noteworthy fact that majority of the people who so object are
responsible for C++ compilers with disproportionatly large Abstraction
Penalty.

The structure of the benchmark is really quite simple. It adds 2000
doubles in an array 25000 times. It does it in 13 different ways that
introduce more and more abstract ways of doing it:

0 - uses simple Fortran-like for loop.
1 - 12 use STL style accumulate template function with plus function object.
1, 3, 5, 7 ,9, 11 use doubles.
2, 4, 6, 8, 10, 12 use Double - double wrapped in a class.
1, 2 - use regular pointers.
3, 4 - use pointers wrapped in a class.
5, 6 - use pointers wrapped in a reverse-iterator adaptor.
7, 8 - use wrapped pointers wrapped in a reverse-iterator adaptor.
9, 10 - use pointers wrapped in a reverse-iterator adaptor wrapped in a
  reverse-iterator adaptor.
11, 12 - use wrapped pointers wrapped in a reverse-iterator adaptor wrapped
  in a reverse-iterator adaptor.

All the operators on Double and different pointer-like classes are
declared inline. The only thing that is really measured is the penalty
for data abstraction. While templates are used, they do not cause any
performance degradation. They are used only to simplify the code.

Since many of you are interested in the C++ performance issues, I
decided to post the benchmark here. I would appreciate if you run it
and (if possible) send me the results indicating what you have
compiled it with (CPU, clock rate, compiler, optimization level). It
is self contained and written so that it could be compiled even with
those compilers that at present cannot compile STL at all.

It takes a fairly long time to run - on a really slow machine it might
take a full hour. (For those of you who want to run it faster - give
it a command line argument that specifies the number of
iterations. The default is 25000, but it gives an accurate predictions
even with 500 or a thousand.)


Alex Stepanov
stepanov@mti.sgi.com

*/


#include <stddef.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <stdlib.h>

template <class T>
inline int operator!=(const T& x, const T& y) {
  return !(x == y);
}

struct Double {
  double value;
  Double() {}
  Double(const double& x) : value(x) {}
  operator double() { return value; }
};

inline Double operator+(const Double& x, const Double& y) {
  return Double(x.value + y.value);
}

struct double_pointer {
    double* current;
    double_pointer() {}
    double_pointer(double* x) : current(x) {}
    double& operator*() const { return *current; }
    double_pointer& operator++() {
	++current;
	return *this;
    }
    double_pointer operator++(int) {
	double_pointer tmp = *this;
	++*this;
	return tmp;
    }
    double_pointer& operator--() {
	--current;
	return *this;
    }
    double_pointer operator--(int) {
	double_pointer tmp = *this;
	--*this;
	return tmp;
    }
};


inline int operator==(const double_pointer& x,
		      const double_pointer& y) {
    return x.current == y.current;
}

struct Double_pointer {
    Double* current;
    Double_pointer() {}
    Double_pointer(Double* x) : current(x) {}
    Double& operator*() const { return *current; }
    Double_pointer& operator++() {
	++current;
	return *this;
    }
    Double_pointer operator++(int) {
	Double_pointer tmp = *this;
	++*this;
	return tmp;
    }
    Double_pointer& operator--() {
	--current;
	return *this;
    }
    Double_pointer operator--(int) {
	Double_pointer tmp = *this;
	--*this;
	return tmp;
    }
};


inline int operator==(const Double_pointer& x,
		       const Double_pointer& y) {
    return x.current == y.current;
}

template <class RandomAccessIterator, class T>
struct reverse_iterator {
    RandomAccessIterator current;
    reverse_iterator(RandomAccessIterator x) : current(x) {}
    T& operator*() const {
      RandomAccessIterator tmp = current;
      return *(--tmp);
    }
    reverse_iterator<RandomAccessIterator, T>& operator++() {
	--current;
	return *this;
    }
    reverse_iterator<RandomAccessIterator, T> operator++(int) {
      reverse_iterator<RandomAccessIterator, T> tmp = *this;
	++*this;
	return tmp;
    }
    reverse_iterator<RandomAccessIterator, T>& operator--() {
	++current;
	return *this;
    }
    reverse_iterator<RandomAccessIterator, T> operator--(int) {
      reverse_iterator<RandomAccessIterator, T> tmp = *this;
	--*this;
	return tmp;
    }
};

template <class RandomAccessIterator, class T>
inline int operator==(const reverse_iterator<RandomAccessIterator, T>& x,
		      const reverse_iterator<RandomAccessIterator, T>& y) {
    return x.current == y.current;
}

struct {
  double operator()(const double& x, const double& y) {return x + y; }
  Double operator()(const Double& x, const Double& y) {return x + y; }
} plus;


template <class Iterator, class Number>
Number accumulate(Iterator first, Iterator last, Number result) {
  while (first != last) result =  plus(result, *first++);
  return result;
}

int iterations = 25000;
#define SIZE 2000

int current_test = 0;

double result_times[20];

void summarize() {
  printf("\ntest      absolute   additions      ratio with\n");
  printf("number    time       per second     test0\n\n");
  int i;
  double millions = (double(SIZE) * iterations)/1000000.;
  for (i = 0; i < current_test; ++i)
    {
      printf("%2i       %5.2fsec    %5.2fM         %.2f\n",
	     i,
	     result_times[i],
	     millions/result_times[i],
	     result_times[i]/result_times[0]);
      // To make the benchmark into a test-case we check that no
      // version has a severe abstraction penalty.  There will always
      // be measurement errors, and we don't presently avoid all
      // abstraction penalty.  As the compiler improves, 1.2 should
      // gradually be replaced with a smaller value.
      if (result_times[i]/result_times[0] > 1.2)
	abort ();
    }
  double gmean_times = 0.;
  double total_absolute_times = 0.;  // sam added 12/05/95
  double gmean_rate = 0.;
  double gmean_ratio = 0.;
  for (i = 0; i < current_test; ++i) {
    total_absolute_times += result_times[i];  // sam added 12/05/95
    gmean_times += log(result_times[i]);
    gmean_rate  += log(millions/result_times[i]);
    gmean_ratio += log(result_times[i]/result_times[0]);
  } 
  printf("mean:    %5.2fsec    %5.2fM         %.2f\n",
	 exp(gmean_times/current_test),
	 exp(gmean_rate/current_test),
	 exp(gmean_ratio/current_test));
  printf("\nTotal absolute time: %.2f sec\n", total_absolute_times);  // sam added 12/05/95
  printf("\nAbstraction Penalty: %.2f\n\n", exp(gmean_ratio/current_test));
}

clock_t start_time, end_time;

inline void start_timer() { start_time = clock(); }

inline double timer() {
  end_time = clock();
  return (end_time - start_time)/double(CLOCKS_PER_SEC);
}

const double init_value = 3.;



double data[SIZE];

Double Data[SIZE];

inline void check(double result) {
  if (result != SIZE * init_value) printf("test %i failed\n", current_test);
}

void test0(double* first, double* last) {
  start_timer();
  for(int i = 0; i < iterations; ++i) {
    double result = 0;
    for (int n = 0; n < last - first; ++n) result += first[n];
    check(result);
  }
  result_times[current_test++] = timer();
}


template <class Iterator, class T>
void test(Iterator first, Iterator last, T zero) {
  int i;
  start_timer();
  for(i = 0; i < iterations; ++i)
    check(double(accumulate(first, last, zero)));
  result_times[current_test++] = timer();
}

template <class Iterator, class T>
void fill(Iterator first, Iterator last, T value) {
  while (first != last) *first++ = value;
}


double d = 0.;
Double D = 0.;
typedef double* dp;
dp dpb = data;
dp dpe = data + SIZE;
typedef Double* Dp;
Dp Dpb = Data;
Dp Dpe = Data + SIZE;
typedef double_pointer dP;
dP dPb(dpb);
dP dPe(dpe);
typedef Double_pointer DP;
DP DPb(Dpb);
DP DPe(Dpe);
typedef reverse_iterator<dp, double> rdp;
rdp rdpb(dpe);
rdp rdpe(dpb);
typedef reverse_iterator<Dp, Double> rDp;
rDp rDpb(Dpe);
rDp rDpe(Dpb);
typedef reverse_iterator<dP, double> rdP;
rdP rdPb(dPe);
rdP rdPe(dPb);
typedef reverse_iterator<DP, Double> rDP;
rDP rDPb(DPe);
rDP rDPe(DPb);
typedef reverse_iterator<rdp, double> rrdp;
rrdp rrdpb(rdpe);
rrdp rrdpe(rdpb);
typedef reverse_iterator<rDp, Double> rrDp;
rrDp rrDpb(rDpe);
rrDp rrDpe(rDpb);
typedef reverse_iterator<rdP, double> rrdP;
rrdP rrdPb(rdPe);
rrdP rrdPe(rdPb);
typedef reverse_iterator<rDP, Double> rrDP;
rrDP rrDPb(rDPe);
rrDP rrDPe(rDPb);

int main(int argv, char** argc) {
  if (argv > 1) iterations = atoi(argc[1]);
  fill(dpb, dpe, double(init_value));
  fill(Dpb, Dpe, Double(init_value));
  test0(dpb, dpe);
  test(dpb, dpe, d);
  test(Dpb, Dpe, D);
  test(dPb, dPe, d);
  test(DPb, DPe, D);
  test(rdpb, rdpe, d);
  test(rDpb, rDpe, D);
  test(rdPb, rdPe, d);
  test(rDPb, rDPe, D);
  test(rrdpb, rrdpe, d);
  test(rrDpb, rrDpe, D);
  test(rrdPb, rrdPe, d);
  test(rrDPb, rrDPe, D);
  summarize();
  return 0;
}



