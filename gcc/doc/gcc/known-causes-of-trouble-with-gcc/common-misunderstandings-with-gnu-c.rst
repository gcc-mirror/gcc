..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: misunderstandings in C++, surprises in C++, C++ misunderstandings

.. _c++-misunderstandings:

Common Misunderstandings with GNU C++
*************************************

C++ is a complex language and an evolving one, and its standard
definition (the ISO C++ standard) was only recently completed.  As a
result, your C++ compiler may occasionally surprise you, even when its
behavior is correct.  This section discusses some areas that frequently
give rise to questions of this sort.

.. toctree::
  :maxdepth: 2


.. index:: C++ static data, declaring and defining, static data in C++, declaring and defining, declaring static data in C++, defining static data in C++

.. _static-definitions:

Declare and Define Static Members
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When a class has static data members, it is not enough to *declare*
the static member; you must also *define* it.  For example:

.. code-block:: c++

  class Foo
  {
    ...
    void method();
    static int bar;
  };

This declaration only establishes that the class ``Foo`` has an
``int`` named ``Foo::bar``, and a member function named
``Foo::method``.  But you still need to define *both*
``method`` and ``bar`` elsewhere.  According to the ISO
standard, you must supply an initializer in one (and only one) source
file, such as:

.. code-block:: c++

  int Foo::bar = 0;

Other C++ compilers may not correctly implement the standard behavior.
As a result, when you switch to :command:`g++` from one of these compilers,
you may discover that a program that appeared to work correctly in fact
does not conform to the standard: :command:`g++` reports as undefined
symbols any static data members that lack definitions.

.. index:: base class members, two-stage name lookup, dependent name lookup

.. _name-lookup:

Name Lookup, Templates, and Accessing Members of Base Classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The C++ standard prescribes that all names that are not dependent on
template parameters are bound to their present definitions when parsing
a template function or class.The C++ standard just uses the
term 'dependent' for names that depend on the type or value of
template parameters.  This shorter term will also be used in the rest of
this section.

Only names that are dependent are looked up at the point
of instantiation.  For example, consider

.. code-block:: c++

    void foo(double);

    struct A {
      template <typename T>
      void f () {
        foo (1);        // 1
        int i = N;      // 2
        T t;
        t.bar();        // 3
        foo (t);        // 4
      }

      static const int N;
    };

Here, the names ``foo`` and ``N`` appear in a context that does
not depend on the type of ``T``.  The compiler will thus require that
they are defined in the context of use in the template, not only before
the point of instantiation, and will here use ``::foo(double)`` and
``A::N``, respectively.  In particular, it will convert the integer
value to a ``double`` when passing it to ``::foo(double)``.

Conversely, ``bar`` and the call to ``foo`` in the fourth marked
line are used in contexts that do depend on the type of ``T``, so
they are only looked up at the point of instantiation, and you can
provide declarations for them after declaring the template, but before
instantiating it.  In particular, if you instantiate ``A::f<int>``,
the last line will call an overloaded ``::foo(int)`` if one was
provided, even if after the declaration of ``struct A``.

This distinction between lookup of dependent and non-dependent names is
called two-stage (or dependent) name lookup.  G++ implements it
since version 3.4.

Two-stage name lookup sometimes leads to situations with behavior
different from non-template codes.  The most common is probably this:

.. code-block:: c++

    template <typename T> struct Base {
      int i;
    };

    template <typename T> struct Derived : public Base<T> {
      int get_i() { return i; }
    };

In ``get_i()``, ``i`` is not used in a dependent context, so the
compiler will look for a name declared at the enclosing namespace scope
(which is the global scope here).  It will not look into the base class,
since that is dependent and you may declare specializations of
``Base`` even after declaring ``Derived``, so the compiler cannot
really know what ``i`` would refer to.  If there is no global
variable ``i``, then you will get an error message.

In order to make it clear that you want the member of the base class,
you need to defer lookup until instantiation time, at which the base
class is known.  For this, you need to access ``i`` in a dependent
context, by either using ``this->i`` (remember that ``this`` is of
type ``Derived<T>*``, so is obviously dependent), or using
``Base<T>::i``.  Alternatively, ``Base<T>::i`` might be brought
into scope by a ``using`` -declaration.

Another, similar example involves calling member functions of a base
class:

.. code-block:: c++

    template <typename T> struct Base {
        int f();
    };

    template <typename T> struct Derived : Base<T> {
        int g() { return f(); };
    };

Again, the call to ``f()`` is not dependent on template arguments
(there are no arguments that depend on the type ``T``, and it is also
not otherwise specified that the call should be in a dependent context).
Thus a global declaration of such a function must be available, since
the one in the base class is not visible until instantiation time.  The
compiler will consequently produce the following error message:

.. code-block::

    x.cc: In member function `int Derived<T>::g()':
    x.cc:6: error: there are no arguments to `f' that depend on a template
       parameter, so a declaration of `f' must be available
    x.cc:6: error: (if you use `-fpermissive', G++ will accept your code, but
       allowing the use of an undeclared name is deprecated)

To make the code valid either use ``this->f()``, or
``Base<T>::f()``.  Using the :option:`-fpermissive` flag will also let
the compiler accept the code, by marking all function calls for which no
declaration is visible at the time of definition of the template for
later lookup at instantiation time, as if it were a dependent call.
We do not recommend using :option:`-fpermissive` to work around invalid
code, and it will also only catch cases where functions in base classes
are called, not where variables in base classes are used (as in the
example above).

Note that some compilers (including G++ versions prior to 3.4) get these
examples wrong and accept above code without an error.  Those compilers
do not implement two-stage name lookup correctly.

.. index:: temporaries, lifetime of, portions of temporary objects, pointers to

.. _temporaries:

Temporaries May Vanish Before You Expect
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is dangerous to use pointers or references to *portions* of a
temporary object.  The compiler may very well delete the object before
you expect it to, leaving a pointer to garbage.  The most common place
where this problem crops up is in classes like string classes,
especially ones that define a conversion function to type ``char *``
or ``const char *`` ---which is one reason why the standard
``string`` class requires you to call the ``c_str`` member
function.  However, any class that returns a pointer to some internal
structure is potentially subject to this problem.

For example, a program may use a function ``strfunc`` that returns
``string`` objects, and another function ``charfunc`` that
operates on pointers to ``char`` :

.. code-block:: c++

  string strfunc ();
  void charfunc (const char *);

  void
  f ()
  {
    const char *p = strfunc().c_str();
    ...
    charfunc (p);
    ...
    charfunc (p);
  }

In this situation, it may seem reasonable to save a pointer to the C
string returned by the ``c_str`` member function and use that rather
than call ``c_str`` repeatedly.  However, the temporary string
created by the call to ``strfunc`` is destroyed after ``p`` is
initialized, at which point ``p`` is left pointing to freed memory.

Code like this may run successfully under some other compilers,
particularly obsolete cfront-based compilers that delete temporaries
along with normal local variables.  However, the GNU C++ behavior is
standard-conforming, so if your program depends on late destruction of
temporaries it is not portable.

The safe way to write such code is to give the temporary a name, which
forces it to remain until the end of the scope of the name.  For
example:

.. code-block:: c++

  const string& tmp = strfunc ();
  charfunc (tmp.c_str ());

.. _copy-assignment:

Implicit Copy-Assignment for Virtual Bases
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When a base class is virtual, only one subobject of the base class
belongs to each full object.  Also, the constructors and destructors are
invoked only once, and called from the most-derived class.  However, such
objects behave unspecified when being assigned.  For example:

.. code-block:: c++

  struct Base{
    char *name;
    Base(const char *n) : name(strdup(n)){}
    Base& operator= (const Base& other){
     free (name);
     name = strdup (other.name);
     return *this;
    }
  };

  struct A:virtual Base{
    int val;
    A():Base("A"){}
  };

  struct B:virtual Base{
    int bval;
    B():Base("B"){}
  };

  struct Derived:public A, public B{
    Derived():Base("Derived"){}
  };

  void func(Derived &d1, Derived &d2)
  {
    d1 = d2;
  }

The C++ standard specifies that :samp:`Base::Base` is only called once
when constructing or copy-constructing a Derived object.  It is
unspecified whether :samp:`Base::operator=` is called more than once when
the implicit copy-assignment for Derived objects is invoked (as it is
inside :samp:`func` in the example).

G++ implements the 'intuitive' algorithm for copy-assignment: assign all
direct bases, then assign all members.  In that algorithm, the virtual
base subobject can be encountered more than once.  In the example, copying
proceeds in the following order: :samp:`name` (via ``strdup``),
:samp:`val`, :samp:`name` again, and :samp:`bval`.

If application code relies on copy-assignment, a user-defined
copy-assignment operator removes any uncertainties.  With such an
operator, the application can define whether and how the virtual base
subobject is assigned.