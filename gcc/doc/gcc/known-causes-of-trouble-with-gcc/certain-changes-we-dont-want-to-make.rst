..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _non-bugs:

Certain Changes We Don't Want to Make
*************************************

This section lists changes that people frequently request, but which
we do not make because we think GCC is better without them.

* Checking the number and type of arguments to a function which has an
  old-fashioned definition and no prototype.

  Such a feature would work only occasionally---only for calls that appear
  in the same file as the called function, following the definition.  The
  only way to check all calls reliably is to add a prototype for the
  function.  But adding a prototype eliminates the motivation for this
  feature.  So the feature is not worthwhile.

* Warning about using an expression whose type is signed as a shift count.

  Shift count operands are probably signed more often than unsigned.
  Warning about this would cause far more annoyance than good.

* Warning about assigning a signed value to an unsigned variable.

  Such assignments must be very common; warning about them would cause
  more annoyance than good.

* Warning when a non-void function value is ignored.

  C contains many standard functions that return a value that most
  programs choose to ignore.  One obvious example is ``printf``.
  Warning about this practice only leads the defensive programmer to
  clutter programs with dozens of casts to ``void``.  Such casts are
  required so frequently that they become visual noise.  Writing those
  casts becomes so automatic that they no longer convey useful
  information about the intentions of the programmer.  For functions
  where the return value should never be ignored, use the
  :fn-attr:`warn_unused_result` function attribute (see :ref:`function-attributes`).

*
  .. index:: fshort-enums

  Making :option:`-fshort-enums` the default.

  This would cause storage layout to be incompatible with most other C
  compilers.  And it doesn't seem very important, given that you can get
  the same result in other ways.  The case where it matters most is when
  the enumeration-valued object is inside a structure, and in that case
  you can specify a field width explicitly.

* Making bit-fields unsigned by default on particular machines where 'the
  ABI standard' says to do so.

  The ISO C standard leaves it up to the implementation whether a bit-field
  declared plain ``int`` is signed or not.  This in effect creates two
  alternative dialects of C.

  .. index:: fsigned-bitfields, funsigned-bitfields

  The GNU C compiler supports both dialects; you can specify the signed
  dialect with :option:`-fsigned-bitfields` and the unsigned dialect with
  :option:`-funsigned-bitfields`.  However, this leaves open the question of
  which dialect to use by default.

  Currently, the preferred dialect makes plain bit-fields signed, because
  this is simplest.  Since ``int`` is the same as ``signed int`` in
  every other context, it is cleanest for them to be the same in bit-fields
  as well.

  Some computer manufacturers have published Application Binary Interface
  standards which specify that plain bit-fields should be unsigned.  It is
  a mistake, however, to say anything about this issue in an ABI.  This is
  because the handling of plain bit-fields distinguishes two dialects of C.
  Both dialects are meaningful on every type of machine.  Whether a
  particular object file was compiled using signed bit-fields or unsigned
  is of no concern to other object files, even if they access the same
  bit-fields in the same data structures.

  A given program is written in one or the other of these two dialects.
  The program stands a chance to work on most any machine if it is
  compiled with the proper dialect.  It is unlikely to work at all if
  compiled with the wrong dialect.

  Many users appreciate the GNU C compiler because it provides an
  environment that is uniform across machines.  These users would be
  inconvenienced if the compiler treated plain bit-fields differently on
  certain machines.

  Occasionally users write programs intended only for a particular machine
  type.  On these occasions, the users would benefit if the GNU C compiler
  were to support by default the same dialect as the other compilers on
  that machine.  But such applications are rare.  And users writing a
  program to run on more than one type of machine cannot possibly benefit
  from this kind of compatibility.

  This is why GCC does and will treat plain bit-fields in the same
  fashion on all types of machines (by default).

  There are some arguments for making bit-fields unsigned by default on all
  machines.  If, for example, this becomes a universal de facto standard,
  it would make sense for GCC to go along with it.  This is something
  to be considered in the future.

  (Of course, users strongly concerned about portability should indicate
  explicitly in each bit-field whether it is signed or not.  In this way,
  they write programs which have the same meaning in both C dialects.)

*
  .. index:: ansi, std

  Undefining ``__STDC__`` when :option:`-ansi` is not used.

  Currently, GCC defines ``__STDC__`` unconditionally.  This provides
  good results in practice.

  Programmers normally use conditionals on ``__STDC__`` to ask whether
  it is safe to use certain features of ISO C, such as function
  prototypes or ISO token concatenation.  Since plain :command:`gcc` supports
  all the features of ISO C, the correct answer to these questions is
  'yes'.

  Some users try to use ``__STDC__`` to check for the availability of
  certain library facilities.  This is actually incorrect usage in an ISO
  C program, because the ISO C standard says that a conforming
  freestanding implementation should define ``__STDC__`` even though it
  does not have the library facilities.  :samp:`gcc -ansi -pedantic` is a
  conforming freestanding implementation, and it is therefore required to
  define ``__STDC__``, even though it does not come with an ISO C
  library.

  Sometimes people say that defining ``__STDC__`` in a compiler that
  does not completely conform to the ISO C standard somehow violates the
  standard.  This is illogical.  The standard is a standard for compilers
  that claim to support ISO C, such as :samp:`gcc -ansi`---not for other
  compilers such as plain :command:`gcc`.  Whatever the ISO C standard says
  is relevant to the design of plain :command:`gcc` without :option:`-ansi` only
  for pragmatic reasons, not as a requirement.

  GCC normally defines ``__STDC__`` to be 1, and in addition
  defines ``__STRICT_ANSI__`` if you specify the :option:`-ansi` option,
  or a :option:`-std` option for strict conformance to some version of ISO C.
  On some hosts, system include files use a different convention, where
  ``__STDC__`` is normally 0, but is 1 if the user specifies strict
  conformance to the C Standard.  GCC follows the host convention when
  processing system include files, but when processing user files it follows
  the usual GNU C convention.

* Undefining ``__STDC__`` in C++.

  Programs written to compile with C++-to-C translators get the
  value of ``__STDC__`` that goes with the C compiler that is
  subsequently used.  These programs must test ``__STDC__``
  to determine what kind of C preprocessor that compiler uses:
  whether they should concatenate tokens in the ISO C fashion
  or in the traditional fashion.

  These programs work properly with GNU C++ if ``__STDC__`` is defined.
  They would not work otherwise.

  In addition, many header files are written to provide prototypes in ISO
  C but not in traditional C.  Many of these header files can work without
  change in C++ provided ``__STDC__`` is defined.  If ``__STDC__``
  is not defined, they will all fail, and will all need to be changed to
  test explicitly for C++ as well.

* Deleting 'empty' loops.

  Historically, GCC has not deleted 'empty' loops under the
  assumption that the most likely reason you would put one in a program is
  to have a delay, so deleting them will not make real programs run any
  faster.

  However, the rationale here is that optimization of a nonempty loop
  cannot produce an empty one. This held for carefully written C compiled
  with less powerful optimizers but is not always the case for carefully
  written C++ or with more powerful optimizers.
  Thus GCC will remove operations from loops whenever it can determine
  those operations are not externally visible (apart from the time taken
  to execute them, of course).  In case the loop can be proved to be finite,
  GCC will also remove the loop itself.

  Be aware of this when performing timing tests, for instance the
  following loop can be completely removed, provided
  ``some_expression`` can provably not change any global state.

  .. code-block:: c++

    {
       int sum = 0;
       int ix;

       for (ix = 0; ix != 10000; ix++)
          sum += some_expression;
    }

  Even though ``sum`` is accumulated in the loop, no use is made of
  that summation, so the accumulation can be removed.

* Making side effects happen in the same order as in some other compiler.

  .. index:: side effects, order of evaluation, order of evaluation, side effects

  It is never safe to depend on the order of evaluation of side effects.
  For example, a function call like this may very well behave differently
  from one compiler to another:

  .. code-block:: c++

    void func (int, int);

    int i = 2;
    func (i++, i++);

  There is no guarantee (in either the C or the C++ standard language
  definitions) that the increments will be evaluated in any particular
  order.  Either increment might happen first.  ``func`` might get the
  arguments :samp:`2, 3`, or it might get :samp:`3, 2`, or even :samp:`2, 2`.

* Making certain warnings into errors by default.

  Some ISO C testsuites report failure when the compiler does not produce
  an error message for a certain program.

  .. index:: pedantic-errors

  ISO C requires a 'diagnostic' message for certain kinds of invalid
  programs, but a warning is defined by GCC to count as a diagnostic.  If
  GCC produces a warning but not an error, that is correct ISO C support.
  If testsuites call this 'failure', they should be run with the GCC
  option :option:`-pedantic-errors`, which will turn these warnings into
  errors.
