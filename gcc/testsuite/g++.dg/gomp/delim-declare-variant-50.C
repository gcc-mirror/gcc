/* { dg-do compile } */

/* Test for restrictions on declare variant functions on virtual functions,
   constructors, and destructors.  */

struct S0
{
  virtual void f_virtual_before0 () {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  virtual void f_virtual_before0 () {}  // { dg-error "declare variant directives are not allowed on virtual functions" }
  #pragma omp end declare variant

  virtual void f_virtual_before1 () {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  void f_virtual_before1 () {}  // { dg-error "declare variant directives are not allowed on virtual functions" }
  #pragma omp end declare variant

  void f_virtual_before2 () {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  virtual void f_virtual_before2 () {}  // { dg-error "declare variant directives are not allowed on virtual functions" }
  #pragma omp end declare variant

  void f_virtual_before3 () {}
  // code elision, no error
  #pragma omp begin declare variant match (implementation={vendor("cray")})
  virtual void f_virtual_before3 () {}
  #pragma omp end declare variant

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  virtual void f_virtual_after0 () {}  // { dg-error "declare variant directives are not allowed on virtual functions" }
  #pragma omp end declare variant
  virtual void f_virtual_after0 () {}

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  void f_virtual_after1 () {}  // { dg-error "declare variant directives are not allowed on virtual functions" }
  #pragma omp end declare variant
  virtual void f_virtual_after1 () {}

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  virtual void f_virtual_after2 () {}  // { dg-error "declare variant directives are not allowed on virtual functions" }
  #pragma omp end declare variant
  void f_virtual_after2 () {}
};

struct S_before {
  S_before() {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_before() {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant

  S_before(int) {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_before(int) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant

  S_before(double) {}
  // code elision, no error
  #pragma omp begin declare variant match (implementation={vendor("cray")})
  S_before(double) {}
  #pragma omp end declare variant
  
  template<typename T>
  S_before(T) {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  template<typename T>
  S_before(T) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant

  ~S_before() {}
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  ~S_before() {}  // { dg-error "declare variant directives are not allowed on destructors" }
  #pragma omp end declare variant
};

struct S_after {
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_after() {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant
  S_after() {}

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_after(int) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant
  S_after(int) {}

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  template<typename T>
  S_after(T) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant
  template<typename T>
  S_after(T) {}

  // code elision, no error
  #pragma omp begin declare variant match (implementation={vendor("cray")})
  ~S_after() {}
  #pragma omp end declare variant
  ~S_after() {}
};

