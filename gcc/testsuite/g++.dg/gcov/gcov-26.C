/* A collection of C++ specific constructs with coverage disabled.  */

/* { dg-options "--coverage" } */
/* { dg-do run } */

#include <stdexcept>

void noop () {}
void noop (int) {}

void
throws (int i)
{
  if (i)
    throw 1;
}

void
throws_stdexcept (int i)
{
    switch (i)
      {
      case 1: throw std::length_error("length error");
      case 2: throw std::domain_error("domain error");
      case 3: throw std::runtime_error("runtime error");
      default: return;
      }
}


/* We can disable coverage for statements inside try/catch blocks.  */
void
try_catch1 ()
{
  try
    {
#pragma GCC suppress_coverage begin
      throws (0); /* count(#) */
#pragma GCC suppress_coverage end
      throws (0); /* count(1) */
    }
  catch (...)
    {
      noop ();    /* count(=====) */
    }

  try
    {
      throws (1); /* count(1) */
      throws (0); /* count(#####) */
    }
  catch (...)
    {
      noop (); /* count (1) */
    }
}

void
try_catch2 ()
{
  try
    {
      throws (0); /* count(1) */
#pragma GCC suppress_coverage begin
      throws (0); /* count(#) */
#pragma GCC suppress_coverage end
    }
  catch (...)
    {
      noop ();    /* count(=====) */
    }

  try
    {
      throws (1); /* count(1) */
      throws (0); /* count(#####) */
    }
  catch (...)
    {
      noop (); /* count (1) */
    }
}

void
try_catch3 ()
{
  try
    {
      throws (0); /* count(1) */
      throws (0); /* count(1) */
    }
  catch (...)
    {
      noop ();    /* count(=====) */
    }

  try
    {
      throws (1); /* count(1) */
      throws (0); /* count(#####) */
    }
  catch (...)
    {
#pragma GCC suppress_coverage begin
      noop (); /* count (#) */
#pragma GCC suppress_coverage end
    }
}

/* We can disable try-catch altogether.  */
void
try_catch4 ()
{
#pragma GCC suppress_coverage begin
  try
    {
      throws (0); /* count(#) */
      throws (0); /* count(#) */
    }
  catch (...)
    {
      noop ();    /* count(#) */
    }
#pragma GCC suppress_coverage end

  try
    {
      throws (1); /* count(1) */
      throws (0); /* count(#####) */
    }
  catch (...)
    {
      noop (); /* count (#) */
    }
}

void
try_catch5 ()
{
  try
    {
      throws (0); /* count(1) */
      throws (0); /* count(1) */
    }
  catch (...)
    {
      noop ();    /* count(=====) */
    }

#pragma GCC suppress_coverage begin
  try
    {
      throws (1); /* count(#) */
      throws (0); /* count(#) */
    }
  catch (...)
    {
      noop (); /* count (#) */
    }
#pragma GCC suppress_coverage end
}

void
try_catch6 ()
{
  try
    {
      throws (0); /* count(1) */
      throws (0); /* count(1) */
    }
  catch (...)
    {
      noop ();    /* count(=====) */
    }

  try
    {
      throws (1); /* count(1) */
      throws (0); /* count(#####) */
    }
  catch (...)
    {
#pragma GCC suppress_coverage begin
      {
	noop (); /* count (#) */
	noop (); /* count (#) */
      }
#pragma GCC suppress_coverage end
    }
}

void
try_catch7 ()
{
#pragma GCC suppress_coverage begin
  try
    {
      throws (0); /* count(#) */
      throws (0); /* count(#) */
    }
  catch (...)
    {
      noop ();    /* count(#) */
    }
#pragma GCC suppress_coverage end
}

void
try_catch8 ()
{
#pragma GCC suppress_coverage begin
  try
    {
      throws_stdexcept (0);	/* count(#) */
      throws_stdexcept (1);	/* count(#) */
    }
  catch (std::length_error&)
    {
      noop (1);		/* count(#) */
    }
  catch (std::domain_error&)
    {
      noop (2);		/* count(#) */
    }
  catch (std::runtime_error&)
    {
      noop (3);		/* count(#) */
    }
#pragma GCC suppress_coverage end

  try
    {
      throws_stdexcept (1);	/* count(1) */
      throws_stdexcept (0);	/* count(#####) */
    }
  catch (std::length_error&)
    {
#pragma GCC suppress_coverage begin
      noop (4);		/* count(#) */
#pragma GCC suppress_coverage end
    }
  catch (std::domain_error&)
    {
      noop (5);		/* count(=====) */
    }
  catch (std::runtime_error&)
    {
      noop (6);		/* count(=====) */
    }

  try
    {
      throws_stdexcept (2);	/* count(1) */
      throws_stdexcept (0);	/* count(#####) */
    }
  catch (std::length_error&)
    {
      noop (7);		/* count(=====) */
    }
  catch (std::domain_error&)
    {
#pragma GCC suppress_coverage begin
      noop (8);		/* count(#) */
#pragma GCC suppress_coverage end
    }
  catch (std::runtime_error&)
    {
      noop (9);		/* count(=====) */
    }
}

/* We can start/stop coverage in different try/catch blocks, even across
   different expressions.  */
void
try_catch9 ()
{
  try
    {
      throws (0); /* count(1) */
#pragma GCC suppress_coverage begin
      throws (0); /* count(#) */
    }
  catch (...)
    {
      noop ();    /* count(#) */
    }

  try
    {
      throws (1); /* count(#) */
      throws (0); /* count(#) */
    }
  catch (...)
    {
      noop (); /* count(#) */
#pragma GCC suppress_coverage end
      noop (); /* count(1) */
    }
}

/* Throws are disabled, either directly or through its surrounding block.  */
int
ifelse_throw1 (int f)
{
#pragma GCC suppress_coverage begin
  if (f >= 2)		/* count(#) */
    throw 1;		/* count(#) */
#pragma GCC suppress_coverage end

  return f;		/* count(1) */
}

int
ifelse_throw2 (int f)
{
  if (f >= 2)		/* count(2) */
#pragma GCC suppress_coverage begin
    throw 1;		/* count(#) */
#pragma GCC suppress_coverage end

  return f;		/* count(1) */
}

int
ifelse_throw3 (int f)
{
#pragma GCC suppress_coverage begin
  if (f >= 2)		/* count(#) */
    throw 1;		/* count(#) */
#pragma GCC suppress_coverage end

  return f;		/* count(1) */
}

int
ifelse_throw4 (int f)
{
  if (f >= 2)		/* count(2) */
#pragma GCC suppress_coverage begin
    throw 1;		/* count(#) */
#pragma GCC suppress_coverage end

  return f;		/* count(1) */
}

int ctor_x;
/* Disabling coverage for a default constructor/initialization.  */
void
ctor1 ()
{
  class C
  {
    int v;
  public:
    C() : v(5) {}
  };

#pragma GCC suppress_coverage begin
  C c;		/* count(#) */
#pragma GCC suppress_coverage end
  // arbitrary action between ctor+dtor
  ctor_x = 1;	/* count(1) */
}

/* Disabling coverage for a constructor/initialization with args.  */
void
ctor2 (int a)
{
  class C
  {
  public:
    explicit C (int e) : v (e) {}
    int v;
  };

#pragma GCC suppress_coverage begin
  C c (a);	/* count(#) */
#pragma GCC suppress_coverage end
  // arbitrary action between ctor+dtor
  ctor_x = 1;	/* count(1) */
}

void
ctor3 ()
{
  class C
  {
    int v;
  public:
    C() : v(5) {}
  };

#pragma GCC suppress_coverage begin
  C c;		/* count(#) */
#pragma GCC suppress_coverage end
  ctor_x = 1;	/* count(1) */
}

/* Disabling coverage for a constructor/initialization with args.  */
void
ctor4 (int a)
{
  class C
  {
  public:
    explicit C (int e) : v (e) {}
    int v;
  };

#pragma GCC suppress_coverage begin
  C c (a);	/* count(#) */
#pragma GCC suppress_coverage end
  ctor_x = 1;	/* count(1) */
}


template <typename T>
T
incr1 (T v)
{
#pragma GCC suppress_coverage begin
  v += 1;	/* count(#) */
#pragma GCC suppress_coverage end
  return v;
}

template <typename T>
T
incr2 (T v)
{
#pragma GCC suppress_coverage begin
  v += 2;	/* count(#) */
#pragma GCC suppress_coverage end
  return v;
}

#pragma GCC suppress_coverage begin
template <typename T>
T
incr3 (T v)
{
  v += 3;	/* count(#) */
#pragma GCC suppress_coverage end
  return v;
}

template <typename T>
__attribute__((suppress_coverage))
T
decr1 (T v)
{
  v += 1;	/* count(#) */
  return v;	/* count(#) */
}

template <typename T>
[[gnu::suppress_coverage]]
T
decr2 (T v)
{
  v += 1;	/* count(#) */
  return v;	/* count(#) */
}

template <typename T>
__attribute__((suppress_coverage))
void
templated_function_level_class (T a)
{
  class C
  {
  public:
    /* Function-level classes would still be counted.  */
    explicit C (T e) : v (e) {}
    T v;
  };

  C c (a);	/* count(#) */
  ctor_x = 1;	/* count(#) */
}

#pragma GCC suppress_coverage begin
template <typename T>
void
templated_function_level_class_pragma (T a)
{
  class C
  {
  public:
    /* Function-level will now be suppressed.  */
    explicit C (T e) : v (e) {} /* count (#) */
    T v;
  };

  C c (a);	/* count(#) */
  ctor_x = 1;	/* count(#) */
}
#pragma GCC suppress_coverage end

int main ()
{
  try_catch1 ();
  try_catch2 ();
  try_catch3 ();
  try_catch4 ();
  try_catch5 ();
  try_catch6 ();
  try_catch7 ();
  try_catch8 ();
  try_catch9 ();

  try { ifelse_throw1 (1); } catch (...) {}
  try { ifelse_throw1 (2); } catch (...) {}

  try { ifelse_throw2 (1); } catch (...) {}
  try { ifelse_throw2 (2); } catch (...) {}

  try { ifelse_throw3 (1); } catch (...) {}
  try { ifelse_throw3 (2); } catch (...) {}

  try { ifelse_throw4 (1); } catch (...) {}
  try { ifelse_throw4 (2); } catch (...) {}

  ctor1 ();
  ctor2 (5);
  ctor3 ();
  ctor4 (5);

  incr1 <int> (1);
  incr1 <double> (2.0);
  incr2 <int> (1);
  incr2 <double> (2.0);
  incr3 <int> (1);
  incr3 <double> (3.0);

  decr1 <int> (1);
  decr1 <double> (2.0);
  decr2 <int> (1);
  decr2 <double> (2.0);

  templated_function_level_class <int> (5);
  templated_function_level_class <double> (5.0);

  templated_function_level_class_pragma <int> (5);
  templated_function_level_class_pragma <double> (5.0);
}

/* { dg-final { run-gcov gcov-26.C } } */
